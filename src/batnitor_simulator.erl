-module(batnitor_simulator).
-behaviour(gen_server).

-include("batnitor.hrl").
-include("common.hrl").
-include_lib("wx/include/wx.hrl").

-export([
         start_link/0,
         stop/0
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, 
        {
         gui_ref = none
        }).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).


init(_) ->
    ?I("~s up and running.", [?MODULE]),
    erlang:process_flag(trap_exit, true),

    ets:new(ets_role_rec, [named_table, public, {keypos, #role.key}, set]),
    ets:new(ets_role_misc_rec, [named_table, public, {keypos, 1}, set]),
    data_mon_group:init_ets(),
    data_mon_attr:init_ets(),

    {wx_ref, _, _, GPID} = batnitor_gui:start_link(),
    State = #state{gui_ref = GPID},
    {ok, State}.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast({set_role_list, RoleMiscList}, State) ->
    ets:delete_all_objects(ets_role_rec),
    ets:delete_all_objects(ets_role_misc_rec),
    {RoleList, MiscList} = lists:unzip(RoleMiscList),
    ets:insert(ets_role_rec, RoleList),
    ets:insert(ets_role_misc_rec, MiscList),
    {noreply, State};

handle_cast({set_monster_group_list, MonsterGroupList}, State) ->
    data_mon_group:set(MonsterGroupList),
    {noreply, State};

handle_cast({set_monster_attr_list, MonsterAttrList}, State) ->
    data_mon_attr:set(MonsterAttrList),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'EXIT', GPID, normal}, #state{gui_ref = GPID} = State) ->
    init:stop(),
    {noreply, State};

handle_info(_Msg, State) ->
    ?I("_Msg = ~w", [_Msg]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ?I("~s shutting down.", [?MODULE]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

