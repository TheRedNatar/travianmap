-module(travianmap_mapline).

-export([parse_line/1]).

-define(bytes_line, 32).


-type grid_position() :: integer(). %% positive_integer
-type x_position() :: integer().
-type y_position() :: integer().
-type tribe() :: 0..7.
-type village_id() :: integer(). %% positive_integer
-type village_name() :: binary().
-type player_id() :: integer(). %% positive_integer
-type player_name() :: binary().
-type alliance_id() :: integer(). %% positive_integer
-type alliance_name() :: binary().
-type population() :: integer(). %% positive_integer
-type region() :: binary() | nil.
-type undef_1() :: boolean() | nil.
-type undef_2() :: boolean() | nil.
-type victory_points() :: integer() | nil.

-type travian_record() :: {
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
			undef_1(),
			undef_2(),
			victory_points()}.
	


-spec parse_line(DirtyLine :: binary()) -> {ok, travian_record()} | {error, any()}.
parse_line(DirtyLine) ->
    Info_Size = byte_size(DirtyLine) - ?bytes_line,
    <<"INSERT INTO `x_world` VALUES (", Info:Info_Size/binary,");">> = DirtyLine,

    case has_minimun_fourteen_comas_policy(Info) of
	exact -> try_line(fun parse_exact/1, Info);
	more -> try_line(fun parse_more/1, Info);
	less -> {error, "broken line, not enought comas"}
    end.

-spec parse_exact(Info :: binary()) -> travian_record().
parse_exact(Info) ->
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
     Undef1,
     Undef2,
     VictoryPoints] = Trav_Tuple,

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
     travian_to_region(Region),
     travian_to_undef1(Undef1),
     travian_to_undef2(Undef2),
     travian_to_victory_points(VictoryPoints)}.



-spec parse_more(Info :: binary()) -> travian_record().
parse_more(Info) ->
    % You should parse it sequentially
    
    case binary:split(Info, <<"'">>, [global]) of
	Tides = [_Group1, 
		 _Village_Name,
		 _Player_Id_Dirt,
		 _Player_Name,
		 _Alliance_Id_Dirt,
		 _Alliance_Name,
		 _Population_Dirt,
		 _Region,
		 _Group2] -> handle_tides(Tides);
	Normal = [_Group1, 
		  _Village_Name,
		  _Player_Id_Dirt,
		  _Player_Name,
		  _Alliance_Id_Dirt,
		  _Alliance_Name,
		  _Group2] -> handle_normal(Normal)
    end.


-spec handle_normal(Split :: [binary()]) -> travian_record().
handle_normal([Group1, Village_Name, Player_Id_Dirt, Player_Name, Alliance_Id_Dirt, Alliance_Name, Group2]) ->

    Player_Id = binary:replace(Player_Id_Dirt, <<",">>, <<"">>, [global]),
    Alliance_Id = binary:replace(Alliance_Id_Dirt, <<",">>, <<"">>, [global]),
    
    [Grid_Position,
     X_Position,
     Y_Position,
     Tribe,
     Village_Id] = binary:split(Group1, <<",">>, [global, trim_all]),
    

    [Population,
     Region,
     Undef1,
     Undef2,
     VictoryPoints] = binary:split(Group2, <<",">>, [global, trim_all]),
    
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
     travian_to_region(Region),
     travian_to_undef1(Undef1),
     travian_to_undef2(Undef2),
     travian_to_victory_points(VictoryPoints)}.

-spec handle_tides(Split :: [binary()]) -> travian_record().
handle_tides([Group1, Village_Name, Player_Id_Dirt, Player_Name, Alliance_Id_Dirt, Alliance_Name, Population_Dirt, Region, Group2]) ->

    Player_Id = binary:replace(Player_Id_Dirt, <<",">>, <<"">>, [global]),
    Alliance_Id = binary:replace(Alliance_Id_Dirt, <<",">>, <<"">>, [global]),
    Population = binary:replace(Population_Dirt, <<",">>, <<"">>, [global]),
    
    [Grid_Position,
     X_Position,
     Y_Position,
     Tribe,
     Village_Id] = binary:split(Group1, <<",">>, [global, trim_all]),
    

    [Undef1,
     Undef2,
     VictoryPoints] = binary:split(Group2, <<",">>, [global, trim_all]),
    
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
     travian_to_region(Region),
     travian_to_undef1(Undef1),
     travian_to_undef2(Undef2),
     travian_to_victory_points(VictoryPoints)}.

-spec travian_to_region(Region :: binary()) -> binary() | nil.
travian_to_region(<<"NULL">>) -> nil;
travian_to_region(Region) -> Region.


-spec travian_to_undef1(Undef1 :: binary()) -> boolean() | nil.
travian_to_undef1(<<"NULL">>) -> nil;
travian_to_undef1(<<"TRUE">>) -> true;
travian_to_undef1(<<"FALSE">>) -> false.


-spec travian_to_undef2(Undef2 :: binary()) -> boolean() | nil.
travian_to_undef2(<<"NULL">>) -> nil;
travian_to_undef2(<<"TRUE">>) -> true;
travian_to_undef2(<<"FALSE">>) -> false.

-spec travian_to_victory_points(VictoryPoints :: binary()) -> integer() | nil.
travian_to_victory_points(<<"NULL">>) -> nil;
travian_to_victory_points(StringPoints) -> binary_to_integer(StringPoints).


-spec has_minimun_fourteen_comas_policy(Info :: binary()) -> exact | more | less.
has_minimun_fourteen_comas_policy(Info) ->
    case length(binary:matches(Info, <<",">>)) of
	14 -> exact;
	X when X > 14 -> more;
	_ -> less
    end.

     

-spec try_line(Parser :: function(), Info :: binary()) -> {ok, travian_record()} | {error, any()}.
try_line(Parser, Info) ->
    try Parser(Info) of
	Record -> {ok, Record}
    catch
	error:Error -> {error, Error}
    end.

				     

    


    
    
