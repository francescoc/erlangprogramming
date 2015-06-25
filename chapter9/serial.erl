%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(serial).

-export([treeToList/1,tree1/0,listToTree/1,test1/0,test2/0,test3/0]).

treeToList({leaf,N}) ->
    [2,N];
treeToList({node,T1,T2}) ->
    TTL1 = treeToList(T1),
    [Size1|_] = TTL1,
    TTL2 = treeToList(T2),
    [Size2|_] = TTL2,
    [Size1+Size2+1|TTL1++TTL2].

listToTree([2,N]) ->
    {leaf,N};
listToTree([_|Code]) ->
    case Code of
	[M|_] ->
	    {Code1,Code2} = lists:split(M,Code),
	    {node,
	     listToTree(Code1),
	     listToTree(Code2)
	     }
    end.



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
    tree1() == listToTree(treeToList(tree1())).
