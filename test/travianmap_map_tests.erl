-module(travianmap_map_tests).
-include_lib("eunit/include/eunit.hrl").


parse_travianmap_nofilter_test() ->
    Input = <<"INSERT INTO `x_world` VALUES (160424,-177,-200,6,22378,'New village',930,'HJU',100,'TINTS',352,NULL);
INSERT INTO `x_world` VALUES (160430,-171,-200,6,24341,'New village',930,'HJU',100,'TINTS',174,NULL);
INSERT INTO `x_world` VALUES (160702,101,-200,2,24217,'ROSIORI',773,'Bury',52,'Chevy',91,NULL);
INSERT INTO `x_world` VALUES (160730,129,-200,1,24680,'Mama',3675,'Papa',233,'ULG',85,NULL);
INSERT INTO `x_world` VALUES (160768,167,-200,3,24520,'ZEUS',1320,'Aria',4,'PARS',173,NULL);">>,
    Output = [{ok,{160424,-177,-200,6,22378,<<"New village">>,930,
		   <<"HJU">>,100,<<"TINTS">>,352}},
	      {ok,{160430,-171,-200,6,24341,<<"New village">>,930,
		   <<"HJU">>,100,<<"TINTS">>,174}},
	      {ok,{160702,101,-200,2,24217,<<"ROSIORI">>,773,<<"Bury">>,
		   52,<<"Chevy">>,91}},
	      {ok,{160730,129,-200,1,24680,<<"Mama">>,3675,<<"Papa">>,233,
		   <<"ULG">>,85}},
	      {ok,{160768,167,-200,3,24520,<<"ZEUS">>,1320,<<"Aria">>,4,
		   <<"PARS">>,173}}],
    ?assertEqual(Output, travianmap_map:parse_map_nofilter(Input)).
    

parse_travianmap_test() ->
    Input = <<"INSERT INTO `x_world` VALUES (160424,-177,-200,6,22378,'New village',930,'HJU',100,'TINTS',352,NULL);
INSERT INTO `x_world` VALUES (160430,-171,-200,6,24341,'New village',930,'HJU',100,'TINTS',174,NULL);
INSERT INTO `x_world` VALUES (160702,101,-200,2,24217,'ROSIORI',773,'Bury',52,'Chevy',91,NULL);
INSERT INTO `x_world` VALUES (160730,129,-200,1,24680,'Mama',3675,'Papa',233,'ULG',85,NULL);
INSERT INTO `x_world` VALUES (160768,167,-200,3,24520,'ZEUS',1320,'Aria',4,'PARS',173,NULL);">>,
    Output = [{160424,-177,-200,6,22378,<<"New village">>,930,
	 <<"HJU">>,100,<<"TINTS">>,352},
	      {160430,-171,-200,6,24341,<<"New village">>,930,
	 <<"HJU">>,100,<<"TINTS">>,174},
	      {160702,101,-200,2,24217,<<"ROSIORI">>,773,<<"Bury">>,
	 52,<<"Chevy">>,91},
	      {160730,129,-200,1,24680,<<"Mama">>,3675,<<"Papa">>,233,
	 <<"ULG">>,85},
	      {160768,167,-200,3,24520,<<"ZEUS">>,1320,<<"Aria">>,4,
		   <<"PARS">>,173}],
    ?assertEqual(Output, travianmap_map:parse_map(Input)).



