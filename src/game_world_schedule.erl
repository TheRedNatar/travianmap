-module(game_world_schedule).

-export([get_servers/0, parse_line/1]).
-export_type([server_metadata/0]).

-type server_metadata() :: #{
    url := binary(),
    name => binary(),
    speed => pos_integer(),
    version => pos_integer(),
    number_of_tribes => pos_integer(),
    start_date => binary(),
    timezone => binary(),
    timezone_offset => integer(),
    artifacts_date => binary(),
    building_plans_date => binary(),
    end_date => binary() | wonder
}.

-spec get_servers() -> {ok, [server_metadata()]} | {error, any()}.
get_servers() ->
    case get_body() of
        {ok, Body} ->
            {ok, parse_body(Body)};
        {error, Reason} ->
            {error, Reason}
    end.

get_body() ->
    Options = [{body_format, binary}, {full_result, false}],
    case httpc:request(get, {"https://blog.travian.com/gameworld-schedule/", []}, [], Options) of
        {ok, {200, Body}} ->
            {ok, Body};
        {ok, {StatusCode, _Body}} ->
            {error, {bad_status_code, StatusCode}};
        {error, Reason} ->
            {error, Reason}
    end.

parse_body(Body) ->
    AllLines = binary:split(Body, <<"\n">>, [global, trim_all]),
    FilteredLines =
        lists:filter(
            fun(X) -> binary:match(X, <<"https://tracking.traviangames.com">>) /= nomatch end,
            AllLines
        ),

    lists:map(fun(X) -> parse_line(X) end, FilteredLines).

parse_line(Line) ->
    {ok, Sax, []} = htmerl:sax(Line),
    Functions = [
        fun get_redirected_url/3,
        fun get_name/3,
        fun get_speed/3,
        fun get_version/3,
        fun get_number_of_tribes/3,
        fun get_start_date/3,
        fun get_timezone/3,
        fun get_artifacts_date/3,
        fun get_building_plans_date/3,
        fun get_end_date/3
    ],
    {_, _, ServerMetadata, Logs} = lists:foldl(fun try_parse/2, {Line, Sax, #{}, []}, Functions),
    case maps:is_key(url, ServerMetadata) of
        true -> ServerMetadata;
        _ -> {error, Logs}
    end.

try_parse(F, {Line, Sax, ServerMetadata, Logs}) ->
    try F(Line, Sax, ServerMetadata) of
        NewServerMetadata -> {Line, Sax, NewServerMetadata, Logs}
    catch
        _:Error -> {Line, Sax, ServerMetadata, [Logs || Error]}
    end.

get_name(_Line, Sax, ServerMetadata) ->
    {characters, Name} = lists:nth(9, Sax),
    maps:put(name, Name, ServerMetadata).

get_speed(_Line, Sax, ServerMetadata) ->
    {characters, SpeedWithX} = lists:nth(11, Sax),
    Speed = list_to_integer(binary_to_list(binary:replace(SpeedWithX, <<"x">>, <<"">>))),
    maps:put(speed, Speed, ServerMetadata).

get_version(_Line, Sax, ServerMetadata) ->
    {characters, Version} = lists:nth(12, Sax),
    maps:put(version, Version, ServerMetadata).

get_number_of_tribes(_Line, Sax, ServerMetadata) ->
    {characters, NumberOfTribes} = lists:nth(13, Sax),
    maps:put(number_of_tribes, list_to_integer(binary_to_list(NumberOfTribes)), ServerMetadata).

get_start_date(_Line, Sax, ServerMetadata) ->
    {characters, <<Day:2/binary, "/", Month:2/binary, "/", Year:4/binary>>} = lists:nth(14, Sax),
    maps:put(start_date, <<Year:4/binary, "-", Month:2/binary, "-", Day:2/binary>>, ServerMetadata).

get_timezone(_Line, Sax, ServerMetadata) ->
    {characters, <<_Time:3/binary, "UTC", Sign:1/binary, Offset/binary>>} = lists:nth(16, Sax),
    Server1 = maps:put(timezone, <<"UTC">>, ServerMetadata),
    case Sign of
        <<"+">> -> maps:put(timezone_offset, list_to_integer(binary_to_list(Offset)), Server1);
        <<"-">> -> maps:put(timezone_offset, -list_to_integer(binary_to_list(Offset)), Server1)
    end.

get_artifacts_date(_Line, Sax, ServerMetadata) ->
    {characters, <<Day:2/binary, "/", Month:2/binary, "/", Year:4/binary>>} = lists:nth(18, Sax),
    maps:put(
        artifacts_date, <<Year:4/binary, "-", Month:2/binary, "-", Day:2/binary>>, ServerMetadata
    ).

get_building_plans_date(_Line, Sax, ServerMetadata) ->
    {characters, <<Day:2/binary, "/", Month:2/binary, "/", Year:4/binary>>} = lists:nth(19, Sax),
    maps:put(
        building_plans_date,
        <<Year:4/binary, "-", Month:2/binary, "-", Day:2/binary>>,
        ServerMetadata
    ).

get_end_date(_Line, Sax, ServerMetadata) ->
    case lists:nth(20, Sax) of
        {characters, <<Day:2/binary, "/", Month:2/binary, "/", Year:4/binary>>} ->
            maps:put(
                artifacts_date,
                <<Year:4/binary, "-", Month:2/binary, "-", Day:2/binary>>,
                ServerMetadata
            );
        {characters, <<"WW Level 100">>} ->
            maps:put(end_date, wonder, ServerMetadata)
    end.

get_redirected_url(Line, _Sax, ServerMetadata) ->
    [_, Part1] = binary:split(Line, <<"<a href=">>),
    [Url | _] = binary:split(Part1, [<<" ">>, <<"rel=noopener">>], [global, trim_all]),
    [_, _, _MaybeUniqueID, _] = binary:split(Url, <<"/">>, [global, trim_all]),

    {ok, RedirectedUrl} = get_server_url_from_redirect(Url),
    maps:put(url, RedirectedUrl, ServerMetadata).

get_server_url_from_redirect(TableUrl) when is_binary(TableUrl) ->
    RedirectUrl = <<TableUrl/binary, "/">>,

    Options = [],
    HttpOptions = [{autoredirect, false}],

    IsLocation =
        fun
            ({"location", _}) ->
                true;
            (_) ->
                false
        end,

    case httpc:request(get, {binary:bin_to_list(RedirectUrl), []}, HttpOptions, Options) of
        {ok, {_StatusLine, HttpHeaders, _HttpBodyResult}} when is_list(HttpHeaders) ->
            case lists:filter(IsLocation, HttpHeaders) of
                [{"location", RedirectedUrl}] when is_binary(RedirectedUrl) ->
                    [ServerUrl, _] = string:split(RedirectedUrl, "/", trailing),
                    {ok, list_to_binary(ServerUrl)};
                X ->
                    {error, {location_not_founded, X}}
            end;
        {error, Reason} ->
            {error, Reason};
        X ->
            {error, X}
    end.
