%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(macros1).
-include("person.hrl").
-export([tstFun/2,birthday/1,test1/0]).

-define(Multiple(X,Y),X rem Y == 0).

tstFun(Z,W) when ?Multiple(Z,W) -> true;
tstFun(Z,W)                     -> false.

%-define(DBG(Str, Args), ok).
-define(DBG(Str, Args), io:format(Str, Args)).

birthday(#person{age=Age} = P) ->
    ?DBG("in records1:birthday(~p)~n", [P]),
    P#person{age=Age+1}.

-define(VALUE(Call),io:format("~p = ~p~n",[??Call,Call])).
test1() -> ?VALUE(length([1,2,3])).

