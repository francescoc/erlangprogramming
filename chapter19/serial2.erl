%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(serial2).

-export([treeToList/1,
         tree0/0,tree1/0,
         listToTree/1]).

-include_lib("eunit/include/eunit.hrl").


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

listToTree([_|Ls]) ->
    lTT(Ls).

%lTT([2,N]) ->
%    {leaf,N};
lTT([N]) ->
    {leaf,N};
lTT([M|Rest] = Code) ->
	    {Code1,Code2} = lists:split(M-1,Rest),
	    {node,
	     lTT(Code1),
	     lTT(Code2)
	     }.

tree0() ->
    {leaf, ant}.

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
    {node,tree1(),tree1()}.

tree3() ->
    {node,tree2(),tree1()}.

leaf_test() ->
    ?assertEqual(tree0() , listToTree(treeToList(tree0()))).

leaf_value_test() ->
    ?assertEqual([2,ant] , treeToList(tree0())).

leaf_negative_test() ->
    ?assertError(badarg, listToTree([1,ant])).

node_test() ->
    ?assertEqual(tree1() , listToTree(treeToList(tree1()))).

node_value_test() ->
    ?assertEqual([11,8,2,cat,5,2,dog,2,emu,2,fish] , treeToList(tree1())).
    
node_negative_test() ->
    ?assertError(badarg, listToTree([8,6,2,cat,2,dog,emu,fish])).

node3_test() ->
    ?assertEqual(tree3() , listToTree(treeToList(tree3()))).
