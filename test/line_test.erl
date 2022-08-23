-module(line_test).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop_parse_line() ->
    ?FORALL(Record, gen_record(), travianmap_mapline:parse_line(query_from_record(Record)) =:= Record).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
query_from_record(
    {grid_position,
     x_position,
     y_position,
     tribe,
     village_id,
     village_name,
     player_id,
     player_name,
     alliance_id,
     alliance_name,
     population,
     region,
     is_capital,
     is_city,
     victory_points}) ->

    <<"INSERT INTO `x_world` VALUES (",
      integer_to_binary(grid_position),
      integer_to_binary(x_position),
      integer_to_binary(y_position),
      integer_to_binary(tribe),
      integer_to_binary(village_id),
      <<"'">> ,
      village_name,
      <<"'">> ,
      integer_to_binary(player_id),
      <<"'">> ,
      player_name,
      <<"'">> ,
      integer_to_binary(alliance_id),
      <<"'">> ,
      alliance_name,
      <<"'">> ,
      integer_to_binary(population),
      is_capital,
      is_city,
      victory_points,
      ");">>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen_boolnil() ->
    ?LET(N, integer(0,2), map_boolnil(N)).
map_boolnil(N) ->
    case N of
	0 -> <<"NULL">>;
	1 -> <<"TRUE">>;
	2 -> <<"FALSE">>
    end.


gen_vic_points() ->
    ?LET(N, {integer(0,1), non_neg_integer()}, map_vic_points(N)).
map_vic_points(N, Points) ->
    case N of
	0 -> <<"NULL">>;
	1 -> integer_to_binary(Points)
    end.

gen_name() ->
    ?LET(Name, binary(), remove_invalids(Name)).
remove_invalids(Name) ->
    binary:replace(Name, <<"'">>, <<"">>, [global]).


    
    

gen_record() ->
    ?LET(Record,
	 {non_neg_integer(),
	  integer(),
	  integer(),
	  integer(0,6),
	  pos_integer(),
	  gen_name(),
	  pos_integer(),
	  gen_name,
	  pos_integer(),
	  gen_name(),
	  pos_integer(),
	  gen_name(),
	  gen_boolnil(),
	  gen_boolnil(),
	  gen_vic_points()}, Record).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
