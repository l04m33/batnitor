-module(dragon_logger).

-compile([export_all]).

-ifdef(DEBUG).

debug_msg(Tag, Module, Line, ID, Format, Args) ->
    error_logger:info_msg("~w:~s:~w:~w: " ++ Format ++ "~n", [Tag, Module, Line, ID | Args]).

info_msg(Tag, Module, Line, ID, Format, Args) ->
    error_logger:info_msg("~w:~s:~w:~w: " ++ Format ++ "~n", [Tag, Module, Line, ID | Args]).

warning_msg(Tag, Module, Line, ID, Format, Args) ->
    error_logger:warning_msg("~w:~s:~w:~w: " ++ Format ++ "~n", [Tag, Module, Line, ID | Args]).

error_msg(Tag, Module, Line, ID, Format, Args) ->
    error_logger:error_msg("~w:~s:~w:~w: " ++ Format ++ "~n", [Tag, Module, Line, ID | Args]).

critical_msg(Tag, Module, Line, ID, Format, Args) ->
    error_logger:error_msg("~w:~s:~w:~w: " ++ Format ++ "~n", [Tag, Module, Line, ID | Args]).

-else.

debug_msg(_, _, _, _, _, _) -> ok.
info_msg(_, _, _, _, _, _) -> ok.
warning_msg(_, _, _, _, _, _) -> ok.
error_msg(_, _, _, _, _, _) -> ok.
critical_msg(_, _, _, _, _, _) -> ok.

-endif.

