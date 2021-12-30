-module(travianmap_travian_status).

-export([get_urls/0, get_map/1]).

-spec get_urls() -> {ok, [binary()]} | {error, any()}.
get_urls() ->
    case get_travian_status() of
	{ok, Body} -> {ok, parse_body(Body)};
	{error, Reason} -> {error, Reason}
    end.


-spec get_travian_status() -> {ok, binary()} | {error, any()}.
get_travian_status() ->
    Options = [{body_format, binary},
	      {full_result, false}],
    case httpc:request(get, {"https://status.travian.com/", []}, [], Options) of
	{ok, {200, Body}} -> {ok, Body};
	{ok, {StatusCode, _Body}} -> {error, {bad_status_code, StatusCode}};
	{error, Reason} -> {error, Reason}
    end.

-spec parse_body(Body :: binary()) -> [binary()].
parse_body(Body) ->
    Splited = binary:split(Body, <<"\n">>, [global, trim_all]),
    GLines = lists:filter(fun(Line) -> binary:match(Line, <<"https">>) /= nomatch end, Splited),
    lists:map(fun get_url/1, GLines).


-spec get_url(Line :: binary()) -> binary().
get_url(Line) ->
    Size = byte_size(Line) - 33,
    % <<"      <td bgcolor=\"#FFFFFF\">", Url:Size/binary, "</td>">> = Line,
    <<_:28/binary, Url:Size/binary, _:5/binary>> = Line,
    Url.



-spec get_map(Url :: binary()) -> {ok, binary()} | {error, any()}.
get_map(Url) ->
    Options = [{body_format, binary},
	      {full_result, false}],
    Endpoint = binary:bin_to_list(Url) ++ "/map.sql",
    case httpc:request(get, {Endpoint, []}, [], Options) of
	{ok, {200, Body}} -> {ok, Body};
	{ok, {StatusCode, _Body}} -> {error, {bad_status_code, StatusCode}};
	{error, Reason} -> {error, Reason}
    end.


    
