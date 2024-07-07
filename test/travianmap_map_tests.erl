-module(travianmap_map_tests).

-include_lib("eunit/include/eunit.hrl").

invalid_line_dont_throw_test() ->
    Input = <<"very bad line">>,
    Output = {error, {badmatch, <<"very bad line">>}},
    ?assertEqual(Output, travianmap_map:parse_line(Input)).
    
    

schema_16_normal_line_test() ->
    Input =
        <<"INSERT INTO `x_world` VALUES (11481,52,172,1,49961,'Uusi kylä 6',18323,'sluibaaja',730,'HC',360,NULL,FALSE,NULL,NULL,NULL);">>,

    Output = #{
        grid_position => 11481,
        x_position => 52,
        y_position => 172,
        tribe => 1,
        village_id => 49961,
        village_name => <<"Uusi kylä 6">>,
        player_id => 18323,
        player_name => <<"sluibaaja">>,
        alliance_id => 730,
        alliance_name => <<"HC">>,
        population => 360,
        region => nil,
        is_capital => false,
        is_city => nil,
        has_harbor => nil,
        victory_points => nil
    },

    ?assertEqual({ok, Output}, travianmap_map:parse_line(Input)).

schema_15_normal_line_test() ->
    Input =
        <<"INSERT INTO `x_world` VALUES (11481,52,172,1,49961,'Uusi kylä 6',18323,'sluibaaja',730,'HC',360,NULL,FALSE,NULL,NULL);">>,

    Output = #{
        grid_position => 11481,
        x_position => 52,
        y_position => 172,
        tribe => 1,
        village_id => 49961,
        village_name => <<"Uusi kylä 6">>,
        player_id => 18323,
        player_name => <<"sluibaaja">>,
        alliance_id => 730,
        alliance_name => <<"HC">>,
        population => 360,
        region => nil,
        is_capital => false,
        is_city => nil,
        victory_points => nil
    },

    ?assertEqual({ok, Output}, travianmap_map:parse_line(Input)).

extra_comas_line_test() ->
    Input =
        <<"INSERT INTO `x_world` VALUES (11481,52,172,1,49961,'Uusi,,,,kylä',18323,'sl,,uibaaja',730,'H,,C',360,'Antibes',FALSE,TRUE,TRUE,4);">>,

    Output = #{
        grid_position => 11481,
        x_position => 52,
        y_position => 172,
        tribe => 1,
        village_id => 49961,
        village_name => <<"Uusi,,,,kylä">>,
        player_id => 18323,
        player_name => <<"sl,,uibaaja">>,
        alliance_id => 730,
        alliance_name => <<"H,,C">>,
        population => 360,
        region => <<"Antibes">>,
        is_capital => false,
        is_city => true,
        has_harbor => true,
        victory_points => 4
    },

    ?assertEqual({ok, Output}, travianmap_map:parse_line(Input)).

%% line_with_less_fields_test() ->
%%     Input =
%%         <<"INSERT INTO `x_world` VALUES (3020,'Failure',193,1,24802,'KOR ATEŞ',617,'VİNİTU');">>,
%%     ?assertMatch({error, {error, {no_schema_available, 8}}}, travianmap_map:parse_line(Input)).
