-module(travian_utils).

-export([distance401/4, distance/6, int_to_tribe/1, tribe_to_int/1]).

-spec distance401(X1 :: integer(), Y1 :: integer(), X2 :: integer(), Y2 :: integer()) ->
                     float().
distance401(X1, Y1, X2, Y2)
    when is_integer(X1), is_integer(Y1), is_integer(X2), is_integer(Y2) ->
    distance(401, 401, X1, Y1, X2, Y2).

-spec distance(Width :: integer(),
               Height :: integer(),
               X1 :: integer(),
               Y1 :: integer(),
               X2 :: integer(),
               Y2 :: integer()) ->
                  float().
distance(Width, Height, X1, Y1, X2, Y2)
    when is_integer(Width), Width > 0, is_integer(Height), Height > 0, is_integer(X1),
         is_integer(Y1), is_integer(X2), is_integer(Y2) ->
    DiffX = abs(X1 - X2),
    DiffY = abs(Y1 - Y2),
    math:pow(math:pow(min(DiffX, Width - DiffX), 2) + math:pow(min(DiffY, Width - DiffY), 2),
             0.5).

-spec int_to_tribe(TribeEncoded :: integer()) -> {ok, atom()} | {error, unknown}.
int_to_tribe(TribeEncoded) when is_integer(TribeEncoded) ->
    case TribeEncoded of
        1 ->
            {ok, romans};
        2 ->
            {ok, teutons};
        3 ->
            {ok, gauls};
        4 ->
            {ok, nature};
        5 ->
            {ok, natars};
        6 ->
            {ok, egyptians};
        7 ->
            {ok, huns};
        8 ->
            {ok, spartans};
        _ ->
            {error, unknown}
    end.

-spec tribe_to_int(Tribe :: atom()) -> {ok, integer()} | {error, unknown}.
tribe_to_int(Tribe) when is_atom(Tribe) ->
    case Tribe of
        romans ->
            {ok, 1};
        teutons ->
            {ok, 1};
        gauls ->
            {ok, 1};
        nature ->
            {ok, 1};
        natars ->
            {ok, 1};
        egyptians ->
            {ok, 1};
        huns ->
            {ok, 1};
        spartans ->
            {ok, 1};
        _ ->
            {error, unknown}
    end.
