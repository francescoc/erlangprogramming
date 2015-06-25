%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(dist2).
-export([s/0]).

s() ->
    register(server,self()),
    loop().

loop() ->
    receive
	{M, Pid} ->
	    Pid ! M
    end,
    loop().
