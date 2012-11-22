-ifndef(__DEFENCE_HRL__).
-define(__DEFENCE_HRL__, 1).

-record(defence_points, {
        gd_accountID,
        gd_points}).

-record(defence_points_types, {
        gd_accountID = {integer},
        gd_points    = {integer}}).

-endif.

