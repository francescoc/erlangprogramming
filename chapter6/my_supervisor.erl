%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(my_supervisor).
-export([start_link/2, stop/1]).
-export([init/1]).

start_link(Name, ChildSpecList) ->
  register(Name, spawn_link(my_supervisor, init, [ChildSpecList])), ok.

init(ChildSpecList) ->
  process_flag(trap_exit, true),
  loop(start_children(ChildSpecList)).

start_children([]) -> [];
start_children([{M, F, A} | ChildSpecList]) ->
  case (catch apply(M,F,A)) of
    {ok, Pid} ->
      [{Pid, {M,F,A}}|start_children(ChildSpecList)];
    _ ->
      start_children(ChildSpecList)
  end.

%% The loop of the supervisor waits in a receive clause for EXIT and stop messages. 
%% If a child terminates, the supervisor receives the EXIT signal and restarts the terminated 
%% child, replacing its entry in the list of children stored in the ChildList variable:

restart_child(Pid, ChildList) ->
  {value, {Pid, {M,F,A}}} = lists:keysearch(Pid, 1, ChildList),
  {ok, NewPid} = apply(M,F,A),
  [{NewPid, {M,F,A}}|lists:keydelete(Pid,1,ChildList)].

loop(ChildList) ->
  receive
    {'EXIT', Pid, _Reason} ->
      NewChildList = restart_child(Pid, ChildList),
      loop(NewChildList);
    {stop, From}  ->
      From ! {reply, terminate(ChildList)}
  end.

%% We stop the supervisor by calling the synchronous client function stop/0. Upon receiving the 
%% stop message, the supervisor runs through the ChildList, terminating the children one by one.
%% Having terminated all the children, the atom ok is returned to the process that initiated 
%% the stop call:

stop(Name) ->
  Name ! {stop, self()},
  receive {reply, Reply} -> Reply end.

terminate([{Pid, _} | ChildList]) ->
  exit(Pid, kill),
  terminate(ChildList);
terminate(_ChildList) -> ok.
