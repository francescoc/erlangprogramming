%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(add_two).
-export([start/0, request/1, loop/0]).

start() ->
  process_flag(trap_exit, true),
  Pid = spawn_link(add_two, loop, []),
  register(add_two, Pid),
  {ok, Pid}.

request(Int) ->
  add_two ! {request, self(), Int},
  receive
    {result, Result}       -> Result;
    {'EXIT', _Pid, Reason} -> {error, Reason}
    after 1000             -> timeout
  end.

loop() ->
  receive
    {request, Pid, Msg} ->
       Pid ! {result, Msg + 2}
  end,
  loop().


