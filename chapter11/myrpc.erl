%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(myrpc).

-export([f/1,setup/0,server/0,facLoop/0,fac/1]).

setup() ->
    spawn('bar@Simon-Thompsons-Computer-2',myrpc,server,[]).

server() ->
    register(facserver,self()),
    facLoop().

facLoop() ->
    receive
	{Pid, N} ->
	    Pid ! {ok, fac(N)}
    end,
    facLoop().

f(N) ->
    {facserver, 'bar@Simon-Thompsons-Computer-2'} ! {self(), N},
    receive
	{ok, Res} ->
	    Val = Res
    end,
    io:format("Factorial of ~p is ~p.~n", [N,Val]).

fac(N) when N==0 ->
    1;
fac(N) ->
    N * fac(N-1).
