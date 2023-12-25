-module(travianmap).

-export([parse_map/1, get_map/1, get_servers/0]).

% @doc Parse a binary map and transform the results in to a list of tuples.
-spec parse_map(Binary_Map :: binary()) -> [{ok, travianmap_map:village_record()} | {error, any()}].
parse_map(Binary_Map) when is_binary(Binary_Map) ->
    travianmap_map:parse_map(Binary_Map).

% @doc Fetch the map of a current url.
-spec get_map(Url :: binary()) -> {ok, binary()} | {error, any()}.
get_map(Url) when is_binary(Url) ->
    Options = [{body_format, binary}, {full_result, false}],
    Endpoint = binary:bin_to_list(Url) ++ "/map.sql",
    case httpc:request(get, {Endpoint, []}, [], Options) of
        {ok, {200, Body}} when is_binary(Body) ->
            {ok, Body};
        {ok, {StatusCode, _Body}} ->
            {error, {bad_status_code, StatusCode}};
        {error, Reason} ->
            {error, Reason}
    end.

% @doc Fetch current servers information from https://blog.travian.com/gameworld-schedule
-spec get_servers() -> {ok, [{ok, game_world_schedule:server_metadata()} | {error, [binary()]}]} | {error, any()}.
get_servers() ->
    game_world_schedule:get_servers().
