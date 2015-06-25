%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(serial_tests).

-include_lib("eunit/include/eunit.hrl").

-import(serial, 
	 [treeToList/1,
         tree0/0,tree1/0,
         listToTree/1]).

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

