-module(travianmap_map).

-export([parse_map/1, parse_map_nofilter/1]).

-spec parse_map_nofilter(Binary_Map :: binary()) ->
    [{ok, travianmap_mapline:travian_record()} | {error, any()}].
parse_map_nofilter(Binary_Map) ->
    Parts = binary:split(Binary_Map, <<"\n">>, [global, trim_all]),
    lists:map(fun(X) -> travianmap_mapline:parse_line(X) end, Parts).

-spec parse_map(Binary_Map :: binary()) -> [travianmap_mapline:travian_record()].
parse_map(Binary_Map) ->
    NoFilter = parse_map_nofilter(Binary_Map),
    lists:filtermap(
        fun
            ({ok, Value}) ->
                {true, Value};
            ({error, _}) ->
                false
        end,
        NoFilter
    ).
