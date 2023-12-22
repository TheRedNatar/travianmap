-module(travianmap_map).

-export([parse_map/1, parse_map_nofilter/1, parse_line/1]).

-define(SEPARATOR_CHAR, ",").
-define(ESCAPE_CHAR, "'").
-define(END_CHAR, ");").

-define(SEPARATOR_BIN, <<?SEPARATOR_CHAR>>).
-define(ESCAPE_BIN, <<?ESCAPE_CHAR>>).
-define(END_BIN, <<?END_CHAR>>).

-define(LINE_INTRO, "INSERT INTO `x_world` VALUES (").

-type village_record() :: #{
    grid_position := integer(),
    x_position := integer(),
    y_position := integer(),
    tribe := non_neg_integer(),
    village_id := non_neg_integer(),
    village_name := binary(),
    player_id := non_neg_integer(),
    player_name := binary(),
    alliance_id := non_neg_integer(),
    alliance_name := binary(),
    population := non_neg_integer(),
    region := binary() | nil,
    is_capital := boolean() | nil,
    is_city := boolean() | nil,
    has_harbor => boolean() | nil,
    victory_points := non_neg_integer() | nil
}.

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

-spec parse_line(Line :: binary()) -> {ok, village_record()} | {error, any()}.
parse_line(Line) ->
    <<?LINE_INTRO, NewLine/binary>> = Line,
    case parse_line(NewLine, []) of
        {error, nomatch} ->
            {error, {nomatch, Line}};
        {ok, Values} ->
            case length(Values) of
                16 -> {ok, #{Key => Value || {Key, Value} <- lists:zip(schema_16_harbor_added(), Values)}};
                15 -> {ok, #{Key => Value || {Key, Value} <- lists:zip(schema_15_original(), Values)}};
                X -> {error, {no_schema_available, X}}
            end
    end.

parse_line(Line, Values) ->
    case next_cut(Line) of
        nomatch ->
            %% {error, nomatch};
            {error, nomatch};
        {_, ?END_BIN} ->
            <<Value:(size(Line) - 2)/binary, ?END_CHAR>> = Line,
            {ok, lists:reverse([parse_int_or_bool(Value) | Values])};
        {Pos, ?SEPARATOR_BIN} ->
            <<Value:Pos/binary, ?SEPARATOR_CHAR, NewLine/binary>> = Line,
            NewValues = [parse_int_or_bool(Value) | Values],
            parse_line(NewLine, NewValues);
        {StartPos, ?ESCAPE_BIN} ->
            EndPos = next_scape(Line),
            <<?ESCAPE_CHAR, Value:(EndPos - StartPos - 1)/binary, ?ESCAPE_CHAR, ?SEPARATOR_CHAR,
                NewLine/binary>> = Line,
            NewValues = [Value | Values],
            parse_line(NewLine, NewValues)
    end.

next_cut(Line) ->
    case binary:match(Line, [?SEPARATOR_BIN, ?ESCAPE_BIN, ?END_BIN]) of
        {Pos, Len} -> {Pos, binary:part(Line, Pos, Len)};
        nomatch -> nomatch
    end.

next_scape(<<?ESCAPE_CHAR, Line/binary>>) ->
    case binary:match(Line, ?ESCAPE_BIN) of
        nomatch -> nomatch;
        {Pos, _} -> Pos + 1
    end.

parse_int_or_bool(<<"TRUE">>) -> true;
parse_int_or_bool(<<"FALSE">>) -> false;
parse_int_or_bool(<<"NULL">>) -> nil;
parse_int_or_bool(X) -> list_to_integer(binary_to_list(X)).

schema_16_harbor_added() ->
    [
        grid_position,
        x_position,
        y_position,
        tribe,
        village_id,
        village_name,
        player_id,
        player_name,
        alliance_id,
        alliance_name,
        population,
        region,
        is_capital,
        is_city,
        has_harbor,
        victory_points
    ].

schema_15_original() ->
    [
        grid_position,
        x_position,
        y_position,
        tribe,
        village_id,
        village_name,
        player_id,
        player_name,
        alliance_id,
        alliance_name,
        population,
        region,
        is_capital,
        is_city,
        victory_points
    ].
