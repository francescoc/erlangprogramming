%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(qsort).

-export([qsort/1,test/0]).

qsort([]) -> [];
qsort([X|Xs]) ->
    qsort([Y || Y<-Xs, Y =< X]) ++ [X] ++ qsort([Y || Y<-Xs, Y > X]).


test() ->
    qsort([1,2,4,2,3,6,5,4]).
