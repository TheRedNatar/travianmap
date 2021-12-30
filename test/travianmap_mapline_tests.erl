-module(travianmap_mapline_tests).
-include_lib("eunit/include/eunit.hrl").


parse_line_normal_test() ->
    Input = <<"INSERT INTO `x_world` VALUES (3020,12,193,1,24802,'KOR ATEŞ',617,'VİNİTU',9,'OĞUZ',222,NULL);">>,
    Output = {ok, {3020, 12, 193, 1, 24802, <<"KOR ATEŞ">>, 617, <<"VİNİTU">>, 9, <<"OĞUZ">>, 222}},
    ?assertEqual(Output, travianmap_mapline:parse_line(Input)).


parse_line_bad_normal_test() ->
    Input = <<"INSERT INTO `x_world` VALUES (3020,'Failure',193,1,24802,'KOR ATEŞ',617,'VİNİTU',9,'OĞUZ',222,NULL);">>,
    ?assertMatch({error, _Any}, travianmap_mapline:parse_line(Input)).


parse_line_normal_with_more_comas_test() ->
    Input = <<"INSERT INTO `x_world` VALUES (673,71,199,3,31273,'Al,hambra',50,'Pr,,of',25,'VCTRX',704,NULL);">>,
    Output = {ok, {673, 71, 199 ,3 , 31273, <<"Al,hambra">>, 50, <<"Pr,,of">>, 25, <<"VCTRX">>, 704}},
    ?assertEqual(Output, travianmap_mapline:parse_line(Input)).

parse_line_bad_normal_with_more_comas_test() ->
    Input = <<"INSERT INTO `x_world` VALUES ('Some',71,199,3,31273,'Al,hambra',50,'Pr,,of',25,'VCTRX',704,NULL);">>,
    ?assertMatch({error, _Any}, travianmap_mapline:parse_line(Input)).


parse_line_territories_test() ->
    Input = <<"INSERT INTO `x_world` VALUES (88942,120,-21,1,18711,'Venom777`s village',6544,'Venom777',0,'',212,'Apollonia Pontica',TRUE,FALSE,0);">>,
    Output = {ok, {88942, 120, -21, 1, 18711, <<"Venom777`s village">>, 6544, <<"Venom777">>, 0, <<"">>, 212, <<"Apollonia Pontica">>, true, false, 0}},
    ?assertEqual(Output, travianmap_mapline:parse_line(Input)).


parse_line_bad_territories_test() ->
    Input = <<"INSERT INTO `x_world` VALUES (88942,120,-21,1, 'failure','Venom777`s village',6544,'Venom777',0,'',212,'Apollonia Pontica',TRUE,FALSE,0);">>,
    ?assertMatch({error, _Any}, travianmap_mapline:parse_line(Input)).


    
parse_line_territories_with_more_comas_test() ->
    Input = <<"INSERT INTO `x_world` VALUES (779,177,199,2,31888,'Giedrai,čia,i',5499,'Virtuve',570,'A,,,tlas',726,'Hyperborea',FALSE,FALSE,65);">>,
    Output = {ok, {779, 177, 199, 2, 31888, <<"Giedrai,čia,i">>, 5499, <<"Virtuve">>, 570, <<"A,,,tlas">>, 726, <<"Hyperborea">>, false, false, 65}},
    ?assertEqual(Output, travianmap_mapline:parse_line(Input)).


parse_line_bad_territories_with_more_comas_test() ->
    Input = <<"INSERT INTO `x_world` VALUES (779,177,199,2,31888,'Giedrai,čia,i',5499,'failure','Virtuve',570,'A,,,tlas',726,'Hyperborea',FALSE,FALSE,65);">>,
    ?assertMatch({error, _Any}, travianmap_mapline:parse_line(Input)).


%% bad sample
%% INSERT INTO `x_world` VALUES (54,-147,200,1,24119,'**',5048,'راس حربة',3,'WN.W1',684,NULL);
