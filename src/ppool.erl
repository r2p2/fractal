%% Author: r2p2
%% Created: 25.10.2009
%% Description: Das Modul nimmt eine Funktion und eine Liste aus Argumenten entgegen.
%%              Die Argumente verteilt es dabei auf eine festzulegende Anzahl an Prozessen
%%              und gibt die Ergebnisse in der Reihenfolge der Argumente zurück.
%% Important:   Die Funktion darf keine Seiteneffekte aufweisen. Andernfalls stimmt die
%%              Sortierung nach Argumenten nicht mehr.

-module(ppool).

-export([run/2]).

run(Fun, Arguments) ->
  MyPid = self(),
  Pids = lists:map(
    fun(_X) -> spawn(fun() -> process_pool_loop(MyPid) end) end,
    lists:seq(1, 10)
  ),
  order_results(run(Fun, Arguments, Pids, [], []), Arguments).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run(_Fun, [], Idle, [], Results) ->
  lists:map(
    fun(Pid) -> Pid ! done end,
    Idle
  ),
  Results;

run(Fun, [], Idle, Worker, Results) ->
  catch_result(Fun, [], Idle, Worker, Results);

run(Fun, Arguments, [], Worker, Results) ->
  catch_result(Fun, Arguments, [], Worker, Results);

run(Fun, [Argument|Arguments], [Pid|Idle], Working, Results) ->
  Pid ! {job, Fun, Argument},
  run(Fun, Arguments, Idle, [Pid|Working], Results).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_pool_loop(PPid) ->
  receive
    {job, Fun, Argument} ->
       Result = apply(Fun, Argument),
       PPid ! {self(), {Argument, Result}},
       process_pool_loop(PPid);
    done -> ok
  end.

order_results(_, []) -> [];
order_results(Results, [Argument|T]) ->
  case lists:keysearch(Argument, 1, Results) of
    false -> erlang:error("Argument not found.");
    {value, {_Argument, Result}} ->
      [Result | order_results(Results, T)]
  end.

catch_result(Fun, Arguments, Idle, Worker, Results) ->
  receive
    {Pid, Result} -> 
      io:format("left: ~p args; ~p worker~n", [length(Arguments), length(Worker)]),
      run(Fun, Arguments, [Pid|Idle], lists:delete(Pid, Worker), [Result|Results])
  end.
