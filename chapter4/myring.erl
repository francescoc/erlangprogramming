%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(myring).
-export([start/1, start_proc/2]).

start(Num) ->
  start_proc(Num, self()).

start_proc(0, Pid) ->
  Pid ! ok;

start_proc(Num, Pid) ->
  NPid = spawn(?MODULE, start_proc, [Num-1, Pid]),
  NPid ! ok,
  receive ok -> ok end.
