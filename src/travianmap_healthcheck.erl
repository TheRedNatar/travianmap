-module(travianmap_healthcheck).


-export([is_healthy/1]).



-spec is_healthy(r :: travian_record()) -> boolean().
is_healthy(r) ->
    FChecks = [
	       fun grid_position_is_pos_integer/1,
	       fun tribe_is_between_integer/1,
	       fun village_id_is_pos_integer/1,
	       fun player_id_is_pos_integer/1,
	       fun alliance_id_is_pos_integer/1,
	       fun population_is_pos_integer/1,
	       fun victory_points_is_pos_integer/1
	      ],
    is_healthy(r, FChecks).

-spec is_healthy(r :: travian_record(), FChecks :: [fun((travian_record()) -> boolean())]).
is_healthy(_r, []) ->
    true.

is_healthy(r, [F | Tail]) ->
    case F(r) of
	true -> is_healthy(r, Tail);
	false -> false
    end.


grid_position_is_pos_integer(r :: travian_record()) -> element(1, r) > 0.
tribe_is_between_integer(r :: travian_record()) -> element(4, r) >= 0 and element(4, r) < 7.
village_id_is_pos_integer(r :: travian_record()) -> element(5, r) > 0.
player_id_is_pos_integer(r :: travian_record()) -> element(7, r) > 0.
alliance_id_is_pos_integer(r :: travian_record()) -> element(7, r) > 0.
population_is_pos_integer(r :: travian_record()) -> element(11, r) > 0.
victory_points_is_pos_integer(r :: travian_record()) -> element(15, r) > 0.
    
    
    



