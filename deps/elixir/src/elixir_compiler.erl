-module(elixir_compiler).
-export([get_opts/0, get_opt/1, get_opt/2, string/2, file/1, file_to_path/2]).
-export([core/0, module/3, eval_forms/4]).
-include("elixir.hrl").
-compile({parse_transform, elixir_transform}).

%% Public API

%% Get compilation options.

get_opt(Key) -> get_opt(Key, get_opts()).

get_opt(Key, Dict) ->
  case lists:keyfind(Key, 1, Dict) of
    false -> false;
    { Key, Value } -> Value
  end.

get_opts() ->
  gen_server:call(elixir_code_server, compiler_options).

%% Compiles the given string.

string(Contents, File) when is_list(Contents), is_binary(File) ->
  Previous = get(elixir_compiled),

  try
    put(elixir_compiled, []),
    Forms = elixir_translator:'forms!'(Contents, 1, File, []),
    eval_forms(Forms, 1, [], elixir:scope_for_eval([{file,File}])),
    lists:reverse(get(elixir_compiled))
  after
    put(elixir_compiled, Previous)
  end.

%% Compile a file, return a tuple of module names and binaries.

file(Relative) when is_binary(Relative) ->
  File = filename:absname(Relative),
  { ok, Bin } = file:read_file(File),
  string(unicode:characters_to_list(Bin), File).

%% Compiles a file to the given path (directory).

file_to_path(File, Path) when is_binary(File), is_binary(Path) ->
  Lists = file(File),
  [binary_to_path(X, Path) || X <- Lists],
  Lists.

%% Evaluates the contents/forms by compiling them to an Erlang module.

eval_forms(Forms, Line, Vars, S) ->
  { Module, I } = retrieve_module_name(),
  { Exprs, FS } = elixir_translator:translate(Forms, S),
  ModuleForm    = module_form(Exprs, Line, S#elixir_scope.file, Module, Vars),

  Args = [X || { _, _, _, X } <- Vars],

  %% Pass { native, false } to speed up bootstrap
  %% process when native is set to true
  { module(ModuleForm, S#elixir_scope.file, [{native,false}], true, fun(_, _) ->
    Res = Module:'BOOTSTRAP'(S#elixir_scope.module, Args),
    code:delete(Module),
    case code:soft_purge(Module) of
      true  -> return_module_name(I);
      false -> ok
    end,
    Res
  end), FS }.

%% Internal API

%% Compile the module by forms based on the scope information
%% executes the callback in case of success. This automatically
%% handles errors and warnings. Used by this module and elixir_module.
module(Forms, S, Callback) ->
  Options = case get_opt(debug_info) of
    true -> [debug_info];
    _ -> []
  end,
  module(Forms, S#elixir_scope.file, Options, false, Callback).

module(Forms, File, Options, Bootstrap, Callback) when
    is_binary(File), is_list(Forms), is_list(Options), is_boolean(Bootstrap), is_function(Callback) ->
  Listname = binary_to_list(File),
  case compile:forms([no_auto_import()|Forms], [return,{source,Listname}|Options]) of
    {ok, ModuleName, Binary, Warnings} ->
      format_warnings(Bootstrap, File, Warnings),
      code:load_binary(ModuleName, Listname, Binary),
      Callback(ModuleName, Binary);
    {error, Errors, Warnings} ->
      format_warnings(Bootstrap, File, Warnings),
      format_errors(File, Errors)
  end.

%% Compile core files for bootstrap.
%% Invoked from the Makefile.

core() ->
  application:start(elixir),
  gen_server:call(elixir_code_server, { compiler_options, [{docs,false},{internal,true}] }),
  [core_file(File) || File <- core_main()].

%% HELPERS

no_auto_import() ->
  Bifs = [{ Name, Arity } || { Name, Arity } <- erlang:module_info(exports), erl_internal:bif(Name, Arity)],
  { attribute, 0, compile, { no_auto_import, Bifs } }.

module_form(Exprs, Line, File, Module, Vars) when
    is_binary(File), is_list(Exprs), is_integer(Line), is_atom(Module) ->

  Cons = lists:foldr(fun({ _, _, Var, _ }, Acc) ->
    { cons, Line, { var, Line, Var }, Acc }
  end, { nil, Line }, Vars),

  Args = [{ var, Line, '_@MODULE'}, Cons],

  [
    { attribute, Line, file, { binary_to_list(File), 1 } },
    { attribute, Line, module, Module },
    { attribute, Line, export, [{ 'BOOTSTRAP', 2 }] },
    { function, Line, 'BOOTSTRAP', length(Args), [
      { clause, Line, Args, [], Exprs }
    ] }
  ].

%% Generate module names from code server.

retrieve_module_name() ->
  gen_server:call(elixir_code_server, retrieve_module_name).

return_module_name(I) ->
  gen_server:cast(elixir_code_server, { return_module_name, I }).

%% Receives a module Binary and outputs it in the given path.

binary_to_path({ModuleName, Binary}, CompilePath) ->
  Path = filename:join(CompilePath, atom_to_list(ModuleName) ++ ".beam"),
  ok = file:write_file(Path, Binary),
  Path.

%% CORE FILES COMPILATION

core_file(File) ->
  try
    Lists = file(list_to_binary(File)),
    [binary_to_path(X, "ebin") || X <- Lists],
    io:format("Compiled ~s~n", [File])
  catch
    Kind:Reason ->
      io:format("~p: ~p~nstacktrace: ~p~n", [Kind, Reason, erlang:get_stacktrace()]),
      exit(1)
  end.

core_main() ->
  io:format("!!!! ~s~n", [filename:absname(".")]),
  [
    "lib/kernel.ex",
    "lib/keyword.ex",
    "lib/list.ex",
    "lib/kernel/typespec.ex",    
    "lib/module.ex",
    "lib/record.ex",
    "lib/record/extractor.ex",
    "lib/macro.ex",
    "lib/macro/env.ex",
    "lib/exception.ex",
    "lib/code.ex",
    "lib/protocol.ex",
    "lib/enum.ex",
    "lib/binary/inspect.ex",
    "lib/binary/chars.ex",
    "lib/list/chars.ex",
    "lib/io.ex",
    "lib/file.ex",
    "lib/access.ex",
    "lib/regex.ex",
    "lib/system.ex",
    "lib/kernel/cli.ex",
    "lib/kernel/error_handler.ex",
    "lib/kernel/parallel_compiler.ex",
    "lib/kernel/record_rewriter.ex"
  ].

%% ERROR HANDLING

format_errors(_File, []) ->
  exit({nocompile, "compilation failed but no error was raised"});

format_errors(File, Errors) ->
  lists:foreach(fun ({_, Each}) ->
    lists:foreach(fun (Error) -> elixir_errors:handle_file_error(File, Error) end, Each)
  end, Errors).

format_warnings(Bootstrap, File, Warnings) ->
  lists:foreach(fun ({_, Each}) ->
    lists:foreach(fun (Warning) -> elixir_errors:handle_file_warning(Bootstrap, File, Warning) end, Each)
  end, Warnings).
