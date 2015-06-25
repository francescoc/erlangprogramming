%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson-module(listComps).

-module(listComps).
-export([fromTo/2,splits/1,perms/1]).

fromTo(N,N) ->
    [N];
fromTo(N,M) when M<N -> 
    [];
fromTo(N,M) ->
    [N|fromTo(N+1,M)].

splits([]) ->
    [{[],[]}];
splits([X|Xs] = Ys) ->
    [ {[],Ys} | [ { [X|As] , Bs} || {As,Bs} <- splits(Xs) ] ].

perms([]) ->
    [[]];
perms([X|Xs]) ->
    [ insert(X,As,Bs) || Ps <- perms(Xs),
			 {As,Bs} <- splits(Ps) ].

insert(X,As,Bs) ->
    lists:append([As,[X],Bs]).
