-module(travian_utils).

-export([distance401/4, distance/6]).



-spec distance401(X1 :: integer(), Y1 :: integer(), X2 :: integer(), Y2 :: integer()) -> float().
distance401(X1, Y1, X2, Y2) when 
      is_integer(X1),
      is_integer(Y1),
      is_integer(X2),
      is_integer(Y2) ->
    distance(401, 401, X1, Y1, X2, Y2).

-spec distance(Width :: integer(), Height :: integer(), X1 :: integer(), Y1 :: integer(), X2 :: integer(), Y2 :: integer()) -> float().
distance(Width, Height, X1, Y1, X2, Y2) when
      is_integer(Width),
      Width > 0,
      is_integer(Height),
      Height > 0,
      is_integer(X1),
      is_integer(Y1),
      is_integer(X2),
      is_integer(Y2) ->
    DiffX = abs(X1 - X2),
    DiffY = abs(Y1 - Y2),
    math:pow(
      math:pow(min(DiffX, Width - DiffX), 2) + math:pow(min(DiffY, Width - DiffY), 2),
      0.5).




