-module(travianmap_game_world_schedule).

-export([get_urls/0, get_server_url_from_redirect/1]).

-record(server_metadata,
        {url = nil :: nil | binary(),
         name = nil :: nil | binary(),
         speed = nil :: nil | pos_integer(),
         version = nil :: nil | binary(),
         tribe = nil :: nil | pos_integer(),
         time_ref = nil :: nil | binary(),
         artifacts_date = nil :: nil | calendar:date(),
         building_plans_date = nil :: nil | calendar:date(),
         start_date = nil :: nil | calendar:date(),
         end_date = nil :: nil | calendar:date()}).

-type server_metadata() :: #server_metadata{}.

-spec get_urls() -> {ok, [server_metadata()]} | {error, any()}.
get_urls() ->
    case get_body() of
        {ok, Body} ->
            {ok, parse_body(Body)};
        {error, Reason} ->
            {error, Reason}
    end.

get_body() ->
    Options = [{body_format, binary}, {full_result, false}],
    case httpc:request(get, {"https://blog.travian.com/gameworld-schedule/", []}, [], Options)
    of
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
        lists:filter(fun(X) -> binary:match(X, <<"https://tracking.traviangames.com">>) /= nomatch
                     end,
                     AllLines),
    ServerList =
        lists:map(fun(X) -> get_server_url_from_redirect(get_redirect_url(X)) end, FilteredLines),
    lists:map(fun({_, ServerUrl}) -> ServerUrl end,
              lists:filter(fun({Atom, _}) -> Atom == ok end, ServerList)).

get_redirect_url(Line) ->
    [_, Part1] = binary:split(Line, <<"<a href=">>),
    [RedirectUrl, _] = binary:split(Part1, <<" ">>),
    RedirectUrl.

get_server_url_from_redirect(TableUrl) when is_binary(TableUrl) ->
    RedirectUrl = <<TableUrl/binary, "/">>,

    Options = [],
    HttpOptions = [{autoredirect, false}],

    IsLocation =
        fun ({"location", _}) ->
                true;
            (_) ->
                false
        end,

    case httpc:request(get, {binary:bin_to_list(RedirectUrl), []}, HttpOptions, Options) of
        {ok, {_StatusLine, HttpHeaders, _HttpBodyResult}} ->
            case lists:filter(IsLocation, HttpHeaders) of
                [{"location", RedirectedUrl}] ->
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
