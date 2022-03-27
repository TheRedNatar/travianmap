-module(travianmap_travibot).

-export([get_urls/0]).


-spec get_urls() -> {ok, [binary()]} | {error, any()}.
get_urls() ->
    case get_url(<<"https://servers.travibot.com//">>) of
	{error, Reason} -> {error, Reason};
	{ok, Body} ->
	    MaxPage = get_last_page(Body),
	    Urls = gen_urls(MaxPage),
	    {ok, reduce_urls(Urls)}
    end.



-spec get_url(Url :: binary()) -> {ok, binary()} | {error, any()}.
get_url(Url) ->
    Options = [{body_format, binary},
	      {full_result, false}],
    case httpc:request(get, {binary:bin_to_list(Url), []}, [], Options) of
	{ok, {200, Body}} -> {ok, Body};
	{ok, {StatusCode, _Body}} -> {error, {bad_status_code, StatusCode}};
	{error, Reason} -> {error, Reason}
    end.


-spec get_last_page(Body :: binary()) -> pos_integer().
get_last_page(Body) ->
    [Part1, _] = binary:split(Body, <<"\">last<">>),
    [_, MaxPage] = binary:split(Part1, <<"\">next</a></li> <li><a href=\"?page=">>),
    binary_to_integer(MaxPage).
    
-spec gen_urls(MaxPage :: pos_integer()) -> [binary()].
gen_urls(MaxPage) ->
    [paste(X) || X <- lists:seq(1, MaxPage)].

-spec paste(X :: pos_integer()) -> binary().
paste(X) ->
    Bin = integer_to_binary(X),
    <<"https://servers.travibot.com/?page=", Bin/binary>>.
    
    

-spec reduce_urls(Urls :: [binary()]) -> [binary()].
reduce_urls(Urls) ->
    lists:foldl(fun get_servers_urls/2, [], Urls).

-spec get_servers_urls(Url :: binary(), List :: [binary()]) -> [binary()].
get_servers_urls(Url, List) ->
    case get_url(Url) of
	{error, _Reason} -> List;
	{ok, Body} ->
	    Lines = binary:split(Body, <<"\n">>, [global, trim_all]),
	    ServersUrls = lists:filtermap(fun parse_line/1, Lines),
	    lists:append(ServersUrls, List)
    end.

-spec parse_line(binary()) -> {true, binary()} | false.
parse_line(<<"<td class=\"l\"", Rest/binary>>) ->
    [_| [ServerUrl| _]] = binary:split(Rest, [<<"href=\"">>, <<"/\" target">>], [global, trim_all]),
    {true, ServerUrl};
parse_line(_) ->
    false.
    
