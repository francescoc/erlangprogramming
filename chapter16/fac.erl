%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(fac).
-export([call/1]).

call(X) ->
    {any, 'c1@Simon-Thompsons-Computer-2'} ! {self(), X},
    receive
        {ok, Result} ->
            Result
    end.
