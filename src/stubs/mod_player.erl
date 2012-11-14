-module(mod_player).

-compile([export_all]).

-include("common.hrl").

is_online(_ID) ->
    {true, #player_status{}}.

check_battle(_PID, _BattPID, _TeamateID) ->
    true.

