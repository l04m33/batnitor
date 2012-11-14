-ifndef(__BATNITOR_HRL__).
-define(__BATNITOR_HRL__, 1).

-define(I(Fmt, Args), error_logger:info_msg("~s:~w: " ++ Fmt ++ "~n", [?MODULE, ?LINE | Args])).
-define(I(Fmt),       ?I(Fmt, [])).

-define(W(Fmt, Args), error_logger:warning_msg("~s:~w: " ++ Fmt ++ "~n", [?MODULE, ?LINE | Args])).
-define(W(Fmt),       ?W(Fmt, [])).

-define(E(Fmt, Args),  error_logger:error_msg("~s:~w: " ++ Fmt ++ "~n", [?MODULE, ?LINE | Args])).
-define(E(Fmt),       ?E(Fmt, [])).

-endif.