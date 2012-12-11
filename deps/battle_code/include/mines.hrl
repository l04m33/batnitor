-ifndef(__MINES_HRL__).
-define(__MINES_HRL__, 1).

-record(mine, {
    location = {0, 0, 0},       % {SceneID, X, Y}
    op       = {none, none},    % {Module, Function}，会以这样的形式被调用 Module:Function(PlayerCell :: #player_cell{}, Mine :: #mine{})
    context  = undefined        % anything
}).

-endif.

