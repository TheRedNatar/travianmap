-module(travianmap).

-export([parse_map/2, get_urls/0, get_map/1, get_info/1]).

-type url() :: binary().
-type binary_map() :: binary().

-spec parse_map(Binary_Map :: binary(), filter | no_filter) -> 
[{ok, travianmap_mapline:normal_record() | travianmap_mapline:territory_record()} | {error, any()}] |
[travianmap_mapline:normal_record() | travianmap_mapline:territory_record()].

parse_map(Binary_Map, filter) ->
    travianmap_map:parse_map(Binary_Map);

parse_map(Binary_Map, no_filter) ->
    travianmap_map:parse_map_nofilter(Binary_Map).


-spec get_urls() -> {ok, [url()]} | {error, any()}.
get_urls() ->
    travianmap_travian_status:get_urls().

-spec get_map(Url :: url()) -> {ok, binary_map()} | {error, any()}.
get_map(Url) ->
    travianmap_travian_status:get_map(Url).

-spec get_info(Url :: url()) -> {ok, map()} | {error, any()}.
get_info(Url) ->
    travianmap_info:get_info(Url).

