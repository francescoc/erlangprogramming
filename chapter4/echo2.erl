%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(echo2).
-export([go/0, loop/0]).

go() ->
  register(echo, spawn(echo2, loop, [])),
  echo ! {self(), hello},
  receive
    {_Pid, Msg} ->
      io:format("~w~n",[Msg])
  end.


loop() ->
  receive
    {From, Msg} ->
      From ! {self(), Msg},
      loop();
    stop ->
      true
  end.

