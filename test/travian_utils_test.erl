-module(travian_utils_test).
-include_lib("eunit/include/eunit.hrl").

distance_test() ->
    assert_delta(132.1 ,travian_utils:distance401(4, 132, 0, 0), 0.1),
    assert_delta(2.8 ,travian_utils:distance401(4, 132, 6, 134), 0.1),
    assert_delta(4 ,travian_utils:distance401(4, 132, 4, 128), 0.1),
    assert_delta(208.7 ,travian_utils:distance401(4, 132, -200, -200), 0.1),
    assert_delta(207.8 ,travian_utils:distance401(4, 132, 200, -200), 0.1),
    assert_delta(207.5 ,travian_utils:distance401(4, 132, 200, 200), 0.1),
    assert_delta(208.1 ,travian_utils:distance401(4, 132, -200, 199), 0.1),
    assert_delta(125 ,travian_utils:distance401(4, 132, 6, -144), 0.1).


assert_delta(Expected, Output, Delta) ->
    Diff = abs(Expected - Output),
    ?assert(Diff < Delta).
    
