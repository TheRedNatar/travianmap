-module(travianmap_healthcheck).

-export([is_healthy/1]).

-spec is_healthy(R :: travianmap_mapline:travian_record()) -> true | {false, atom()}.
is_healthy(R) ->
    FChecks =
        [{fun grid_position_is_pos_integer/1, grid_position_is_pos_integer},
         {fun tribe_is_between_integer/1, tribe_is_between_integer},
         {fun village_id_is_non_integer/1, village_id_is_non_integer},
         {fun player_id_is_non_integer/1, player_id_is_non_integer},
         {fun alliance_id_is_non_integer/1, alliance_id_is_non_integer},
         {fun population_is_non_neg_integer/1, population_is_non_neg_integer},
         {fun victory_points_is_non_integer/1, victory_points_is_non_integer}],
    is_healthy(R, FChecks).

-spec is_healthy(R :: travianmap_mapline:travian_record(),
                 FChecks :: [{fun((travianmap_mapline:travian_record()) -> boolean()), atom()}]) ->
                    true | {false, atom()}.
is_healthy(_R, []) ->
    true;
is_healthy(R, [{F, FunName} | Tail]) ->
    case F(R) of
        true ->
            is_healthy(R, Tail);
        false ->
            {false, FunName}
    end.

grid_position_is_pos_integer(R) ->
    element(1, R) > 0.

tribe_is_between_integer(R) ->
    element(4, R) >= 0.

village_id_is_non_integer(R) ->
    element(5, R) >= 0.

player_id_is_non_integer(R) ->
    element(7, R) >= 0.

alliance_id_is_non_integer(R) ->
    element(9, R) >= 0.

population_is_non_neg_integer(R) ->
    element(11, R) >= 0.

victory_points_is_non_integer(R) ->
    element(15, R) >= 0.
