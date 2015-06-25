%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(db).
-export([new/0,write/3,read/2, delete/2,destroy/1]).
-vsn(1.0).

new()                -> dict:new().

write(Key, Data, Db) -> dict:store(Key, Data, Db).

read(Key, Db) ->
  case dict:fetch(Key, Db) of
    error      -> {error, instance};
    {ok, Data} -> {ok, Data}
  end.

delete(Key, Db) -> dict:erase(Key, Db).

destroy(_Db)    -> ok.


