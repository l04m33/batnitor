-ifndef(__GEN_CALLBACK_SERVER_HRL__).
-define(__GEN_CALLBACK_SERVER_HRL__, 1).

-type process_ref()     :: pid() | atom() | {atom(), node()}.
-type gen_server_ref()  :: process_ref() | {global, term()} | {via, module(), term()}.
-type gen_server_name() :: {local, atom()} | {global, term()} | {via, module(), term()}.
-type cb_server_name()  :: pid() | {gen_server_name(), node()}.
-type cb_func()         :: function() | {function(), any()}.

-define(RECEIVE_CB, (fun gen_callback_server:receive_cb/2)).
-define(REPLY_CB,   (fun gen_callback_server:reply_cb/1)).
-define(CLIENT_REPLY_CB(Sender, Protocol), {(fun gen_callback_server:client_reply_cb/1), {Sender, Protocol}}).
-define(MIXED_REPLY_CB(Sender, Protocol),  {(fun gen_callback_server:mixed_reply_cb/1),  {Sender, Protocol}}).

-record(cb_event,
    {
        reply_to  = none :: process_ref() | none,
        cb_arg    = none :: any(),
        msg_ref   = none :: reference() | none,
        sent_from = none :: cb_server_name() | none,
        context   = none :: any()
    }).

-endif.
