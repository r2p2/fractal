%% Author: r2p2
%% Created: 22.10.2009
%% Description: TODO: Add description to fractal
-module(mandelbrot).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([calculate_area/6]).
-export([test/0]).


-define(MAXIMUM_ABSOLUTE_SQARE, 2).
-define(MAXIMUM_ITERATIONS, 256).


%%
%% API Functions
%%

calculate_area(MinX, MinY, MaxX, MaxY, PointDistanceX, PointDistanceY) ->
  area_iteration(MinX, MinY, MaxX, MaxY, PointDistanceX, PointDistanceY).

test() ->
  Field = calculate_area(-2, -1, 1, 1, 0.002, 0.002),
  egd:save(fractal:render(Field), "/tmp/image2.png"),
  init:stop().
  
%%
%% Local Functions
%%

area_iteration(MinX, MinY, MaxX, MaxY, PointDistanceX, PointDistanceY) ->
  Arguments = lists:map(
    fun(Line) ->
      [MinY + Line*PointDistanceY, MinX, MaxX, PointDistanceX]
    end,
    lists:seq(0, round((MaxY - MinY) / PointDistanceY))
  ),
  ppool:run(fun line_iteration/4, Arguments).

line_iteration(Line, MinX, MaxX, PointDistanceX) ->
  lists:map(
    fun(Row) ->
      point_iteration(MinX + Row*PointDistanceX, Line)
    end,
    lists:seq(0, round((MaxX - MinX) / PointDistanceX))
  ).

point_iteration(X, Y) ->
  point_iteration(X, Y, 1, 0, 0).

point_iteration(CX, CY, Iteration, X, Y) ->
  AbsoluteSquare = X*X + Y*Y,
  if
    (AbsoluteSquare =< ?MAXIMUM_ABSOLUTE_SQARE) and (Iteration < ?MAXIMUM_ITERATIONS) ->
      XT = X*X - Y*Y + CX,
      YT = 2*X*Y + CY,
      point_iteration(CX, CY, Iteration+1, XT, YT);
    true ->
      if 
        Iteration == ?MAXIMUM_ITERATIONS -> {CX, CY, {0,0,0}};
        true ->
	 C = round(Iteration - math:log(math:log(AbsoluteSquare) / math:log(4)) / math:log(2)) rem 255,
	 {CX, CY, {C,0,0}}
      end
      % Iteration - math:log(math:log(AbsoluteSquare) / math:log(4)) / math:log(2) % damit es bunt wird
  end.
