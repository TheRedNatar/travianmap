-module(game_world_schedule_tests).

-include_lib("eunit/include/eunit.hrl").

parse_line_test() ->
    Input =
        <<"\t<td class=\"column-1\"><a href=https://tracking.traviangames.com/124811000013100/1 rel=noopener\" target=\"_blank\">europe 9</a></td><td class=\"column-2\">x1</td><td class=\"column-3\">WW</td><td class=\"column-4\">3</td><td class=\"column-5\">04/01/2024</td><td class=\"column-6\"><a href=\"https://dateful.com/convert/utc?t=15\" rel=\"noopener\" target=\"_blank\">15 UTC+0</a></td><td class=\"column-7\">03/04/2024</td><td class=\"column-8\">02/07/2024</td><td class=\"column-9\">WW Level 100</td>">>,
    Output =
        #{
            name => <<"europe 9">>,
            version => <<"WW">>,
            speed => 1,
            url => <<"https://ts9.x1.europe.travian.com">>,
            number_of_tribes => 3,
            start_date => <<"2024-01-04">>,
            timezone => <<"UTC">>,
            timezone_offset => 0,
            artifacts_date => <<"2024-04-03">>,
            building_plans_date => <<"2024-07-02">>,
            end_date => wonder
        },
    ?assertEqual({ok, Output}, game_world_schedule:parse_line(Input)).
