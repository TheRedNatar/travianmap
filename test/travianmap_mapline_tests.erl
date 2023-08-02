-module(travianmap_mapline_tests).
-include_lib("eunit/include/eunit.hrl").


parse_line_test() ->
    Input = <<"INSERT INTO `x_world` VALUES (11481,52,172,1,49961,'Uusi kylä 6',18323,'sluibaaja',730,'HC',360,NULL,FALSE,NULL,NULL,NULL);">>,
    Output = {ok, {11481, 52, 172, 1, 49961, <<"Uusi kylä 6">>, 18323, <<"sluibaaja">>, 730, <<"HC">>, 360, nil, false, nil, nil, nil}},
    ?assertEqual(Output, travianmap_mapline:parse_line(Input)).

parse_line_tides_of_conquest_test() ->
    Input = <<"INSERT INTO `x_world` VALUES (11481,52,172,1,49961,'Uusi kylä 6',18323,'sluibaaja',730,'HC',360,'Antibes',FALSE,TRUE,FALSE,4);">>,
    Output = {ok, {11481, 52, 172, 1, 49961, <<"Uusi kylä 6">>, 18323, <<"sluibaaja">>, 730, <<"HC">>, 360, <<"Antibes">>, false, true, false, 4}},
    ?assertEqual(Output, travianmap_mapline:parse_line(Input)).


parse_line_with_less_fields_test() ->
    Input = <<"INSERT INTO `x_world` VALUES (3020,'Failure',193,1,24802,'KOR ATEŞ',617,'VİNİTU');">>,
    ?assertMatch({error, _Any}, travianmap_mapline:parse_line(Input)).


parse_line_with_more_comas_test() ->
    Input = <<"INSERT INTO `x_world` VALUES (12331,100,170,1,26208,'Ruskij ,,,,,karabl idi..',3129,'oge',41,'INCA',2,NULL,TRUE,NULL,NULL,NULL);">>,
    Output = {ok, {12331, 100, 170 ,1 , 26208, <<"Ruskij ,,,,,karabl idi..">>, 3129, <<"oge">>, 41, <<"INCA">>, 2, nil, true, nil, nil, nil}},
    ?assertEqual(Output, travianmap_mapline:parse_line(Input)).


parse_line_tides_of_conquest_with_more_comas_test() ->
    Input = <<"INSERT INTO `x_world` VALUES (11481,52,172,1,49961,'Uusi,,,,kylä',18323,'sl,,uibaaja',730,'H,,C',360,'Antibes',FALSE,TRUE,TRUE,4);">>,
    Output = {ok, {11481, 52, 172, 1, 49961, <<"Uusi,,,,kylä">>, 18323, <<"sl,,uibaaja">>, 730, <<"H,,C">>, 360, <<"Antibes">>, false, true, true, 4}},
    ?assertEqual(Output, travianmap_mapline:parse_line(Input)).
