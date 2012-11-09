-module(batnitor_gui).
-behaviour(wx_object).

-include("batnitor.hrl").
-include_lib("wx/include/wx.hrl").

-export([
         start_link/0, 
         start_link/1,
         stop/0
        ]).

-export([
         init/1,
         terminate/2,
         code_change/3,
         handle_info/2,
         handle_call/3,
         handle_cast/2,
         handle_event/2
        ]).


start_link() ->
    start_link([]).

start_link(Config) ->
    wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).

stop() ->
    gen_server:cast(?MODULE, stop).


init(Config) ->
    ?INFO("~s up and running.", [?MODULE]),
    erlang:process_flag(trap_exit, true),
    wx:new(Config),
    wx:batch(fun () -> create_main_layout(Config) end).


handle_event(#wx{id = ID, event = #wxCommand{type = command_menu_selected}}, State) ->
    case ID of
        ?wxID_OPEN ->
            void;               % TODO
        ?wxID_EXIT ->
            stop()
    end,
    {noreply, State};

handle_event(WX, State) ->
    ?INFO("wx event: ~w", [WX]),
    {noreply, State}.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ?INFO("~s shutting down.", [?MODULE]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




create_main_layout(_Config) ->
    MainFrame = wxFrame:new(wx:null(), ?wxID_ANY, "Batnitor", [{size, {800, 600}}]),

    create_menu_bar(MainFrame),
    create_status_bar(MainFrame),

    wxFrame:show(MainFrame),
    {MainFrame, no_use}.

create_menu_bar(Frame) ->
    MainMenuBar = wxMenuBar:new(),

    FileMenu = wxMenu:new([]),
    wxMenu:append(FileMenu, ?wxID_OPEN, "&Open Player Config"),
    wxMenu:appendSeparator(FileMenu),
    wxMenu:append(FileMenu, ?wxID_EXIT, "&Quit"),
    wxMenuBar:append(MainMenuBar, FileMenu, "&File"),

    wxFrame:setMenuBar(Frame, MainMenuBar),
    wxFrame:connect(Frame, command_menu_selected).

create_status_bar(Frame) ->
    wxFrame:createStatusBar(Frame, []).

