-module(travianmap).

-export([parse_map/2, get_urls/0, get_urls/1, get_map/1, get_info/1]).

-type url() :: binary().
-type binary_map() :: binary().
-type travian_record() :: travianmap_mapline:travian_record().



% @doc Parse a binary map and transform the results in to a list of tuples.
-spec parse_map(Binary_Map :: binary(), filter | no_filter) -> [{ok, travian_record()} | {error, any()}] | [travian_record()].
parse_map(Binary_Map, filter) ->
    travianmap_map:parse_map(Binary_Map);

parse_map(Binary_Map, no_filter) ->
    travianmap_map:parse_map_nofilter(Binary_Map).


% @doc Get all the current urls of the travian servers.
-spec get_urls() -> {ok, [url()]} | {error, any()}.
get_urls() ->
    get_urls(all).

-spec get_urls(all | travian_status | travibot) -> {ok, [url()]} | {error, any()}.
get_urls(travian_status) ->
    travianmap_travian_status:get_urls();
get_urls(travibot) ->
    travianmap_travibot:get_urls();
get_urls(all) ->
    case travianmap_travian_status:get_urls() of
	{error, Reason1} -> 
	    case travianmap_travibot:get_urls() of
		{error, Reason2} -> {error, {Reason1, Reason2}};
		{ok, Urls} -> {ok, Urls}
	    end;
	{ok, Urls} -> 
	    case travianmap_travibot:get_urls() of
		{error, _Reason} -> Urls;
		{ok, Urls2} -> 
		    Nodups = sets:to_list(
			       sets:from_list(
				 lists:append(Urls, Urls2)
				)
			      ),
		    {ok, Nodups}
	    end
    end.


% @doc Fetch the map of a current url.
-spec get_map(Url :: url()) -> {ok, binary_map()} | {error, any()}.
get_map(Url) ->
    travianmap_travian_status:get_map(Url).

% @doc Fetch the aditional info of a current url.
-spec get_info(Url :: url()) -> {ok, map()} | {error, any()}.
get_info(Url) ->
    travianmap_info:get_info(Url).

