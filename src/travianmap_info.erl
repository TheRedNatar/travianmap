-module(travianmap_info).

-export([get_info/1]).

-spec get_info(Url :: binary()) -> {ok, map()} | {error, any()}.
get_info(Url) ->
    case request_travian_info(Url) of
        {ok, Body} ->
            {ok, parse_body(Body)};
        {error, Reason} ->
            {error, Reason}
    end.

-spec request_travian_info(Url :: binary()) -> {ok, binary()} | {error, any()}.
request_travian_info(Url) ->
    Options = [{body_format, binary}, {full_result, false}],
    Endpoint = binary:bin_to_list(Url) ++ "/login.php",
    case httpc:request(get, {Endpoint, []}, [], Options) of
        {ok, {200, Body}} ->
            {ok, Body};
        {ok, {StatusCode, _Body}} ->
            {error, {bad_status_code, StatusCode}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec parse_body(Body :: binary()) -> map().
parse_body(Body) ->
    FeaturesMaps =
        [
            try_get_features(fun get_game_options/1, Body),
            try_get_features(fun get_map_dims/1, Body),
            try_get_features(fun get_world_id/1, Body),
            try_get_features(fun get_speed/1, Body),
            try_get_features(fun get_country/1, Body),
            try_get_features(fun get_title/1, Body)
        ],
    lists:foldl(fun(X, Acc) -> maps:merge(Acc, X) end, #{}, FeaturesMaps).

-spec get_game_options(Body :: binary()) -> map().
get_game_options(Body) ->
    [_, Part1] = binary:split(Body, <<"var T4_feature_flags = ">>),
    [Json | _] = binary:split(Part1, <<";">>, [trim_all]),
    {ok, Map} = thoas:decode(Json),
    Map.

-spec get_map_dims(Body :: binary()) -> map().
get_map_dims(Body) ->
    [_, Part1] = binary:split(Body, <<"window.TravianDefaults = Object.assign(\n">>),
    [Json | _] = binary:split(Part1, <<",\n">>),
    {ok, Map} = thoas:decode(Json),
    maps:get(<<"Size">>, maps:get(<<"Map">>, Map)).

-spec get_world_id(Body :: binary()) -> map().
get_world_id(Body) ->
    [_, Part1] = binary:split(Body, <<"Travian.Game.worldId = ">>),
    [WorldId | _] = binary:split(Part1, <<";">>),
    #{<<"worldId">> => binary:replace(WorldId, <<"'">>, <<>>, [global])}.

-spec get_speed(Body :: binary()) -> map().
get_speed(Body) ->
    [_, Part1] = binary:split(Body, <<"Travian.Game.speed = ">>),
    [Speed | _] = binary:split(Part1, <<";">>),
    #{<<"speed">> => binary_to_integer(Speed)}.

-spec get_country(Body :: binary()) -> map().
get_country(Body) ->
    [_, Part1] = binary:split(Body, <<"Travian.Game.country = ">>),
    [Country | _] = binary:split(Part1, <<";">>),
    #{<<"country">> => binary:replace(Country, <<"'">>, <<>>, [global])}.

-spec get_title(Body :: binary()) -> map().
get_title(Body) ->
    [_, Part1] = binary:split(Body, <<"<title>">>),
    [Title | _] = binary:split(Part1, <<"</title>">>),
    #{<<"title">> => Title}.

-spec try_get_features(Fun :: function(), Body :: binary()) -> map().
try_get_features(Fun, Body) ->
    try Fun(Body) of
        Map ->
            Map
    catch
        error:_Error ->
            #{}
    end.
