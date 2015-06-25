%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(usr_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) -> 
    usr_sup:start_link().
stop(_State) ->
    ok.
