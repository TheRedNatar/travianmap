-module(travianmap).

-export([parse_map/2, get_urls/0, get_map/1, get_info/1]).

-type travian_record() :: travianmap_mapline:travian_record().



% @doc Parse a binary map and transform the results in to a list of tuples.
-spec parse_map(Binary_Map :: binary(), filter | no_filter) -> [{ok, travian_record()} | {error, any()}] | [travian_record()].
parse_map(Binary_Map, filter) when is_binary(Binary_Map) ->
    travianmap_map:parse_map(Binary_Map);

parse_map(Binary_Map, no_filter) when is_binary(Binary_Map) ->
    travianmap_map:parse_map_nofilter(Binary_Map).


% @doc Get all the current urls of the travian servers.
-spec get_urls() -> {ok, [binary()]} | {error, any()}.
get_urls() ->
    travianmap_game_world_schedule:get_urls().

% @doc Fetch the map of a current url.
-spec get_map(Url :: binary()) -> {ok, binary()} | {error, any()}.
get_map(Url) when is_binary(Url) ->
    Options = [{body_format, binary},
	      {full_result, false}],
    Endpoint = binary:bin_to_list(Url) ++ "/map.sql",
    case httpc:request(get, {Endpoint, []}, [], Options) of
	{ok, {200, Body}} -> {ok, Body};
	{ok, {StatusCode, _Body}} -> {error, {bad_status_code, StatusCode}};
	{error, Reason} -> {error, Reason}
    end.

% @doc Fetch the aditional info of a current url.
-spec get_info(Url :: binary()) -> {ok, map()} | {error, any()}.
get_info(Url) when is_binary(Url) ->
    travianmap_info:get_info(Url).

