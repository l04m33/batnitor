-module(dragon_logger).

-compile([export_all]).

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

