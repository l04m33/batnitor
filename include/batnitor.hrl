-ifndef(__BATNITOR_HRL__).
-define(__BATNITOR_HRL__, 1).

-define(INFO(Fmt, Args), error_logger:info_msg("~s:~w: " ++ Fmt ++ "~n", [?MODULE, ?LINE | Args])).
-define(INFO(Fmt),       ?INFO(Fmt, [])).

-define(WARN(Fmt, Args), error_logger:warning_msg("~s:~w: " ++ Fmt ++ "~n", [?MODULE, ?LINE | Args])).
-define(WARN(Fmt),       ?WARN(Fmt, [])).

-define(ERR(Fmt, Args),  error_logger:error_msg("~s:~w: " ++ Fmt ++ "~n", [?MODULE, ?LINE | Args])).
-define(ERR(Fmt),       ?ERR(Fmt, [])).

-endif.
