-ifndef(__BATNITOR_HRL__).
-define(__BATNITOR_HRL__, 1).

-ifdef(DEBUG).

-define(I(Fmt, Args), error_logger:info_msg("~s:~w: " ++ Fmt ++ "~n", [?MODULE, ?LINE | Args])).
-define(I(Fmt),       ?I(Fmt, [])).

-define(W(Fmt, Args), error_logger:warning_msg("~s:~w: " ++ Fmt ++ "~n", [?MODULE, ?LINE | Args])).
-define(W(Fmt),       ?W(Fmt, [])).

-define(E(Fmt, Args),  error_logger:error_msg("~s:~w: " ++ Fmt ++ "~n", [?MODULE, ?LINE | Args])).
-define(E(Fmt),       ?E(Fmt, [])).

-else.

-define(I(Fmt, Args), ok).
-define(I(Fmt),       ok).

-define(W(Fmt, Args), ok).
-define(W(Fmt),       ok).

-define(E(Fmt, Args), ok).
-define(E(Fmt),       ok).

-endif.

-record(misc_info, {
    key         = {0, 0},
    guai_da_ren = 0,
    ren_da_guai = 0,
    nan_du      = 0,
    guai_lei_xing = 0,
    guai_zhi_ye = 0,
    skills_list = []}).

-endif.
