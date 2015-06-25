%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(my_timer).
-export([send_after/2, sleep/1, send/3]).

send_after(Time, Msg) ->
    spawn(my_timer, send, [self(),Time,Msg]).

send(Pid, Time, Msg) ->
  receive
  after
    Time ->
      Pid ! Msg
  end.

sleep(T) ->
  receive
  after
    T ->  true
  end.
