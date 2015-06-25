%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(math).
-export([add/2]).

add(X,Y) ->
  test_int(X),
  test_int(Y),
  X + Y.

test_int(Int) when is_integer(Int) -> true;
test_int(Int) -> throw({error, {non_integer, Int}}).
