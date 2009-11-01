-module(fractal_server).

-export([start/0, start/6, task_request/0, solution/2]).

% API Functions

start() ->
  start(1, 1, -2, -2, 2, 2).

start(VParts, HParts, MinX, MinY, MaxX, MaxY) ->
  VDist = (MaxY-MinY)/VParts,
  HDist = (MaxX-MinX)/HParts,
  Tasks = lists:flatten(lists:map(
    fun(Row) ->
      lists:map(
        fun(Column) ->
          NMinX = MinX + Column*HDist,
          NMinY = MinY + Row*VDist,
          NMaxX = MinX + (Column+1)*HDist,
          NMaxY = MinY + (Row+1)*VDist,
          {NMinX, NMinY, NMaxX, NMaxY, VDist/100, HDist/100}
        end,
        lists:seq(0, HParts-1)
      )
    end,
    lists:seq(0, VParts-1)
  )),
 io:format("~p~n", [Tasks]), 
  DrawPid = spawn(fun() -> write_loop() end),
  global:register_name(?MODULE, spawn(fun() -> loop(DrawPid, Tasks, []) end)).

task_request() ->
  global:whereis_name(?MODULE) ! {self(), task_request},
  receive
    Return -> Return
  end.

solution(Task, Field) ->
  global:whereis_name(?MODULE) ! {solution, Task, Field}.

% Local Functions

loop(DrawPid, [], TasksInProgress) ->
  receive
    {Pid, task_request} ->
      Pid ! no_tasks_left,
      loop(DrawPid, [], TasksInProgress);
    {solution, MyTask, Field} ->
      DrawPid ! {MyTask, Field},
      loop(DrawPid, [], lists:delete(MyTask, TasksInProgress))
  end;

loop(DrawPid, [Task|T], TasksInProgress) ->
  receive
    {Pid, task_request} ->
      Pid ! {task, Task},
      loop(DrawPid, T, [Task|TasksInProgress]);
    {solution, MyTask, Field} ->
      DrawPid ! {MyTask, Field},
      loop(DrawPid, [Task|T], lists:delete(MyTask, TasksInProgress))
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_loop() ->
  receive
    {_Task, Field} ->
      file:write_file("./images/"++integer_to_list(random:uniform(100000))++".txt", format(Field))
  end,
  write_loop().

format(Fractal) ->
  io:format("ndd~n"),
  Height = length(Fractal),
  Width = length(lists:nth(1, Fractal)),
  Image = egd:create(Width, Height),
  integer_to_list(Width) ++ " " ++ integer_to_list(Height) ++ "\n" ++ format_lines(Image, Fractal, 0).

format_lines(_, [], _) -> "";
format_lines(Image, [Line|T], Y) ->
  format_pixel(Image, Line, 0, Y) ++ format_lines(Image, T, Y+1).

format_pixel(_, [], _, _) -> "";
format_pixel(Image, [{R, G, B}|T], X, Y) -> 
  integer_to_list(R) ++ " " ++ integer_to_list(G) ++ " " ++ integer_to_list(B) ++ "\n" ++ format_pixel(Image, T, X+1, Y).

to_list(V) when is_float(V) -> float_to_list(V);
to_list(V) when is_integer(V) -> integer_to_list(V).



