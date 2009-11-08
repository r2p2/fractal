-module(fractal_server).

-export([start/0, start/6, start_profiling/0, task_request/0, solution/2]).

% API Functions

start() ->
  start(2, 2, -2, -2, 2, 2).

start_profiling() ->
  percept:profile("/tmp/fractal_server_profile.dat", {fractal_server, start, []}, [procs]).

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
          {NMinX, NMinY, NMaxX, NMaxY, VDist/10000, HDist/10000}
        end,
        lists:seq(0, HParts-1)
      )
    end,
    lists:seq(0, VParts-1)
  )),
 io:format("~p~n", [Tasks]), 
  DrawPid = spawn(fun() -> write_loop(1) end),
  global:register_name(?MODULE, spawn(fun() -> loop(DrawPid, Tasks, []) end)).

task_request() ->
  global:whereis_name(?MODULE) ! {self(), task_request},
  receive
    Return -> Return
  end.

solution(Task, Field) ->
    global:whereis_name(?MODULE) ! {solution, Task, list_to_binary(Field)}.

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

write_loop(Round) ->
  receive
    {Task, Field} ->
      {ok, File} = file:open("./images/"++integer_to_list(Round)++".txt", [write, delayed_write]),
      format(Task, Field, File),
      ok = file:close(File)
  end,
  write_loop(Round+1).

format({MinX, MinY, MaxX, MaxY, VR, HR}, Fractal, File) ->
  io:format("file writing started~n"),
  %Height = length(Fractal),
  %Width = length(lists:nth(1, Fractal)),
  file:write(File, list_to_binary(integer_to_list(round((MaxX-MinX)/VR)) ++ " " ++ integer_to_list(round((MaxY-MinY)/HR)) ++ "\n")),
  format_lines(File, Fractal).

format_lines(_, <<>>) -> ok;
format_lines(File, <<Iteration:16, Rest/binary>>) ->
  file:write(File, list_to_binary(integer_to_list(Iteration) ++ "\n")),
  format_lines(File, Rest).
