#!/usr/bin/env escript
%%! -pa ../elixir/ebin

main(_) ->
    ExList = [
        <<"./ex/skill_macro.ex">>,
        <<"./ex/skills.ex">>],
    application:start(elixir),
    gen_server:call(elixir_code_server, {compiler_options, [{docs, true}, {debug_info, true}]}),
    [elixir_compiler:file_to_path(F, <<"./ebin">>) || F <- ExList],
    erlang:halt(0).

