-module(travianmap_mapline).

-export([parse_line/1]).

-define(bytes_normal_line, 37).
-define(bytes_territory_line, 32).


-type grid_position() :: integer(). %% positive_integer
-type x_position() :: integer().
-type y_position() :: integer().
-type tribe() :: 0..7.
-type population() :: integer(). %% positive_integer
-type village_id() :: integer(). %% positive_integer
-type village_name() :: binary().
-type player_id() :: integer(). %% positive_integer
-type player_name() :: binary().
-type alliance_id() :: integer(). %% positive_integer
-type alliance_name() :: binary().


-type region() :: binary().

-type normal_record() :: {
			grid_position(),
			x_position(),
			y_position(),
			tribe(),
			village_id(),
			village_name(),
			player_id(),
			player_name(),
			alliance_id(),
			alliance_name(),
			population()}.


-type territory_record() :: {
			grid_position(),
			x_position(),
			y_position(),
			tribe(),
			village_id(),
			village_name(),
			player_id(),
			player_name(),
			alliance_id(),
			alliance_name(),
			population(),
			region(),
			boolean(),
			boolean(),
			integer()}.


-spec parse_line(DirtyLine :: binary()) -> {ok, normal_record() | territory_record()} | {error, any()}.
parse_line(DirtyLine) ->
    case has_territory_policy(DirtyLine) of
	false -> parse_normal(DirtyLine);
	true -> parse_territory(DirtyLine)
    end.
	


-spec parse_normal(DirtyLine :: binary()) -> {ok, normal_record()} | {error, any()}.
parse_normal(DirtyLine) ->
    Info_Size = byte_size(DirtyLine) - ?bytes_normal_line,
    <<"INSERT INTO `x_world` VALUES (", Info:Info_Size/binary,",NULL);">> = DirtyLine,

    case has_minimun_ten_comas_policy(Info) of
	exact -> try_line(fun parse_normal_exact/1, Info);
	more -> try_line(fun parse_normal_more/1, Info);
	less -> {error, "broken line, not enought comas"}
    end.

-spec parse_normal_exact(Info :: binary()) -> normal_record().
parse_normal_exact(Info) ->
    Trav_Tuple = binary:split(binary:replace(Info, <<"'">>, <<"">>, [global]), <<",">>, [global]),
    [Grid_Position,
     X_Position,
     Y_Position,
     Tribe,
     Village_Id,
     Village_Name,
     Player_Id,
     Player_Name,
     Alliance_Id,
     Alliance_Name,
     Population] = Trav_Tuple,

    {binary_to_integer(Grid_Position),
     binary_to_integer(X_Position),
     binary_to_integer(Y_Position),
     binary_to_integer(Tribe),
     binary_to_integer(Village_Id),
     Village_Name,
     binary_to_integer(Player_Id),
     Player_Name,
     binary_to_integer(Alliance_Id),
     Alliance_Name,
     binary_to_integer(Population)}.
    


-spec parse_normal_more(Info :: binary()) -> normal_record().
parse_normal_more(Info) ->
    % You should parse it sequentially
    [Group1, 
     Village_Name,
     Player_Id_Dirt,
     Player_Name,
     Alliance_Id_Dirt,
     Alliance_Name,
     Population_Dirt] = binary:split(Info, <<"'">>, [global]),
    
    Player_Id = binary:replace(Player_Id_Dirt, <<",">>, <<"">>, [global]),
    Alliance_Id = binary:replace(Alliance_Id_Dirt, <<",">>, <<"">>, [global]),
    Population = binary:replace(Population_Dirt, <<",">>, <<"">>, [global]),
    
    [Grid_Position,
     X_Position,
     Y_Position,
     Tribe,
     Village_Id] = binary:split(Group1, <<",">>, [global, trim_all]),
    
    {binary_to_integer(Grid_Position),
     binary_to_integer(X_Position),
     binary_to_integer(Y_Position),
     binary_to_integer(Tribe),
     binary_to_integer(Village_Id),
     Village_Name,
     binary_to_integer(Player_Id),
     Player_Name,
     binary_to_integer(Alliance_Id),
     Alliance_Name,
     binary_to_integer(Population)}.
    

    
-spec parse_territory(DirtyLine :: binary()) -> {ok, territory_record()} | {error, any()}.
parse_territory(DirtyLine) ->
    Info_Size = byte_size(DirtyLine) - ?bytes_territory_line,
    <<"INSERT INTO `x_world` VALUES (", Info:Info_Size/binary,");">> = DirtyLine,
    case has_minimun_fourteen_comas_policy(Info) of
	exact -> try_line(fun parse_territory_exact/1, Info);
	more -> try_line(fun parse_territory_more/1, Info);
	less -> {error, "broken line, not enought comas"}
    end.



-spec parse_territory_exact(Info :: binary()) -> territory_record().
parse_territory_exact(Info) ->
    Trav_Tuple = binary:split(binary:replace(Info, <<"'">>, <<"">>, [global]), <<",">>, [global]),
    [Grid_Position,
     X_Position,
     Y_Position,
     Tribe,
     Village_Id,
     Village_Name,
     Player_Id,
     Player_Name,
     Alliance_Id,
     Alliance_Name,
     Population,
     Region,
     Bool1,
     Bool2,
     Unkwnon_Value] = Trav_Tuple,

    {binary_to_integer(Grid_Position),
     binary_to_integer(X_Position),
     binary_to_integer(Y_Position),
     binary_to_integer(Tribe),
     binary_to_integer(Village_Id),
     Village_Name,
     binary_to_integer(Player_Id),
     Player_Name,
     binary_to_integer(Alliance_Id),
     Alliance_Name,
     binary_to_integer(Population),
     Region,
     binary_to_bool(Bool1),
     binary_to_bool(Bool2),
     binary_to_integer(Unkwnon_Value)}.


-spec parse_territory_more(Info :: binary()) -> territory_record().
parse_territory_more(Info) ->
    % You should parse it sequentially
    [Group1, 
     Village_Name,
     Player_Id_Dirt,
     Player_Name,
     Alliance_Id_Dirt,
     Alliance_Name,
     Population_Dirt,
     Region,
     Group2] = binary:split(Info, <<"'">>, [global]),
    
    Player_Id = binary:replace(Player_Id_Dirt, <<",">>, <<"">>, [global]),
    Alliance_Id = binary:replace(Alliance_Id_Dirt, <<",">>, <<"">>, [global]),
    Population = binary:replace(Population_Dirt, <<",">>, <<"">>, [global]),
    
    [Grid_Position,
     X_Position,
     Y_Position,
     Tribe,
     Village_Id] = binary:split(Group1, <<",">>, [global, trim_all]),
    

    [Bool1,
     Bool2,
     Unkwnon_Value] = binary:split(Group2, <<",">>, [global, trim_all]),
    
    {binary_to_integer(Grid_Position),
     binary_to_integer(X_Position),
     binary_to_integer(Y_Position),
     binary_to_integer(Tribe),
     binary_to_integer(Village_Id),
     Village_Name,
     binary_to_integer(Player_Id),
     Player_Name,
     binary_to_integer(Alliance_Id),
     Alliance_Name,
     binary_to_integer(Population),
     Region,
     binary_to_bool(Bool1),
     binary_to_bool(Bool2),
     binary_to_integer(Unkwnon_Value)}.



-spec has_territory_policy(DirtyLine :: binary()) -> boolean().
has_territory_policy(DirtyLine) ->
    case binary:match(DirtyLine, [<<"TRUE">>, <<"FALSE">>]) of
	nomatch -> false;
	_ -> true
    end.

-spec has_minimun_ten_comas_policy(Info :: binary()) -> exact | more | less.
has_minimun_ten_comas_policy(Info) ->
    case length(binary:matches(Info, <<",">>)) of
	10 -> exact;
	X when X > 10 -> more;
	_ -> less
    end.


-spec has_minimun_fourteen_comas_policy(Info :: binary()) -> exact | more | less.
has_minimun_fourteen_comas_policy(Info) ->
    case length(binary:matches(Info, <<",">>)) of
	14 -> exact;
	X when X > 14 -> more;
	_ -> less
    end.

     

-spec try_line(Parser :: function(), Info :: binary()) -> {ok, normal_record()} | {error, any()}.
try_line(Parser, Info) ->
    try Parser(Info) of
	Record -> {ok, Record}
    catch
	error:Error -> {error, Error}
    end.

				     
-spec binary_to_bool(Bin :: binary()) -> boolean().
binary_to_bool(<<"TRUE">>) ->
    true;
binary_to_bool(<<"FALSE">>) ->
    false.

    


    
    
