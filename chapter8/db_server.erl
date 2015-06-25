%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(db_server).
-export([start/0, stop/0, upgrade/1]).
-export([write/2, read/1, delete/1]).
-export([init/0, loop/1]).
-vsn(1.0).

start() ->
  register(db_server, spawn(db_server, init, [])).

stop()->
  db_server ! stop.

upgrade(Data) ->
  db_server ! {upgrade, Data}.

write(Key, Data) ->
  db_server ! {write, Key, Data}.

read(Key) ->
  db_server ! {read, self(), Key},
  receive Reply -> Reply end.

delete(Key) ->
  db_server ! {delete, Key}.

init() ->
  loop(db:new()).

loop(Db) ->
  receive
    {write, Key, Data} ->
       loop(db:write(Key, Data, Db));
    {read, Pid, Key} ->
       Pid ! db:read(Key, Db),
       loop(Db);
    {delete, Key} ->
       loop(db:delete(Key, Db));
    {upgrade, Data} ->
      NewDb = db:convert(Data, Db),
      db_server:loop(NewDb);
    stop ->
      db:destroy(Db)
  end. 


