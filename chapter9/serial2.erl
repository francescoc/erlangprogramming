%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(serial2).

-export([treeToList/1,tree1/0,listToTree/1,test1/0,test2/0,test3/0,test4/0,lTT/1]).

treeToList({leaf,N}) ->
    [2,N];
treeToList({node,T1,T2}) ->
    TTL1 = treeToList(T1),
    [Size1|_] = TTL1,
    TTL2 = treeToList(T2),
    [Size2|List2] = TTL2,
    [Size1+Size2|TTL1++List2].

% all that is needed to decode deterministically is the size of the left 
% subtree. That's therefore what is coded by the head. In fact, the head is 
% the size of the whole tree, so that's why in the decoding phase we ditch the
% first datum as it is of no value.

lTT([_|Ls]) ->
    listToTree(Ls).

listToTree([2,N]) ->
    {leaf,N};
listToTree([N]) ->
    {leaf,N};
listToTree([M|Rest] = Code) ->
	    {Code1,Code2} = lists:split(M-1,Rest),
	    {node,
	     listToTree(Code1),
	     listToTree(Code2)
	     }.

tree1() ->
    {node,
     {node,
      {leaf,cat},
      {node,
       {leaf,dog},
       {leaf,emu}
      }
     },
     {leaf,fish}
    }.

tree2() ->
    {node,
     tree1(),
     tree1()
     }.

test1() ->
    treeToList(tree1()).

test2() ->
    treeToList(tree2()).

test3() ->
    tree1() == lTT(treeToList(tree1())).

test4() ->
    tree2() == lTT(treeToList(tree2())).
