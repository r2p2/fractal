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
-define(MAXIMUM_ITERATIONS, 5000).


%%
%% API Functions
%%

calculate_area(MinX, MinY, MaxX, MaxY, PointDistanceX, PointDistanceY) ->
  area_iteration(MinX, MinY, MaxX, MaxY, PointDistanceX, PointDistanceY).

test() ->
  Field = calculate_area(-2, -1, 1, 1, 0.05, 0.05),
  lists:map(
    fun(Line) ->
         lists:map(
           fun(Element) ->
             io:format("~p", [Element])
           end,
           Line
         ),
         io:format("~n")
    end,
    Field
  ),
  erlang:exit(ok).
  
%%
%% Local Functions
%%

area_iteration(MinX, MinY, MaxX, MaxY, PointDistanceX, PointDistanceY) ->
  MyPid = self(),
  Pids = lists:map(
    fun(Line) ->
      spawn(fun() -> line_iteration(MyPid, MinY + Line*PointDistanceY, MinX, MaxX, PointDistanceX) end)
    end,
    lists:seq(0, round((MaxY - MinY) / PointDistanceY))
  ),
  order_results(catch_lines(Pids), Pids).

line_iteration(Pid, Line, MinX, MaxX, PointDistanceX) ->
  Result = lists:map(
    fun(Row) ->
      point_iteration(MinX + Row*PointDistanceX, Line)
    end,
    lists:seq(0, round((MaxX - MinX) / PointDistanceX))
  ),
  Pid ! {self(), Result}.

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
        Iteration == ?MAXIMUM_ITERATIONS -> 1;
        true -> 0
      end
      % Iteration - math:log(math:log(AbsoluteSquare) / math:log(4)) / math:log(2) % damit es bunt wird
  end.

catch_lines([]) ->
  [];
catch_lines([_Pid|T]) ->
  receive
    Result -> io:format("line returned ~p left~n", [length(T)]),
       [Result | catch_lines(T)]
  end.

order_results(_, []) -> [];
order_results(Results, [Pid|T]) ->
  case lists:keysearch(Pid, 1, Results) of
    false -> erlang:error("Pid not found.");
    {value, {Pid, Result}} ->
	[Result | order_results(Results, T)]
  end.
