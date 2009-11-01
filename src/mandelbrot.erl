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
-export([start/0, start_profiling/0, loop/1, test/0]).


-define(MAXIMUM_ABSOLUTE_SQARE, 4).
-define(MAXIMUM_ITERATIONS, 1000).

%%
%% API Functions
%%

start() ->
  net_adm:ping('fs@ares'),
  timer:sleep(1000),
  loop(1),
  init:stop().

start_profiling() ->
  percept:profile("/tmp/mandelbrot_profile.dat", {mandelbrot, start, []}, [procs]).

loop(Round) ->
  case fractal_server:task_request() of
    {task, {MinX, MinY, MaxX, MaxY, PDX, PDY}} ->
      fractal_server:solution({MinX, MinY, MaxX, MaxY, PDX, PDY}, calculate_area(MinX, MinY, MaxX, MaxY, PDX, PDY)),
      loop(Round+1);
    no_tasks_left ->
      ok
  end.

calculate_area(MinX, MinY, MaxX, MaxY, PointDistanceX, PointDistanceY) ->
  area_iteration(MinX, MinY, MaxX, MaxY, PointDistanceX, PointDistanceY).

test() ->
  %Field = calculate_area(-2, -1, 1, 1, 0.002, 0.002),
  Field = calculate_area(-2, -1, 1, 1, 0.005, 0.005),
  %Field = calculate_area(-0.274, -0.872, -0.224, -0.822, 0.00005, 0.00005),
  egd:save(fractal:render(Field), "/tmp/image2"),
  init:stop().
  
%%
%% Local Functions
%%

area_iteration(MinX, MinY, MaxX, MaxY, PointDistanceX, PointDistanceY) ->
  Arguments = lists:map(
    fun(Line) ->
      [MinY + Line*PointDistanceY, MinX, MaxX, PointDistanceX]
    end,
    lists:seq(0, round((MaxY - MinY) / PointDistanceY)-1)
  ),
  ppool:run(fun line_iteration/4, Arguments).

line_iteration(Line, MinX, MaxX, PointDistanceX) ->
  lists:map(
    fun(Row) ->
      point_iteration(MinX + Row*PointDistanceX, Line)
    end,
    lists:seq(0, round((MaxX - MinX) / PointDistanceX)-1)
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
        Iteration == ?MAXIMUM_ITERATIONS -> {0,0,0};
        true ->
	 %C = round(Iteration - math:log(math:log(AbsoluteSquare) / math:log(4)) / math:log(2)) rem 255,
	 %C = round((Iteration - math:log(math:log(AbsoluteSquare) / math:log(4)) / math:log(2))/?MAXIMUM_ITERATIONS * 255),
	 C = round(Iteration/?MAXIMUM_ITERATIONS * 255),
	 {C,C,0}
      end
      % Iteration - math:log(math:log(AbsoluteSquare) / math:log(4)) / math:log(2) % damit es bunt wird
  end.
