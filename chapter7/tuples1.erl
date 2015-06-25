%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(tuples1).
-export([test1/0, test2/0]).

% Simple examples of tuples ...to go with records1.

% -record(person, {name, age=0, phone}).

birthday({N,A,T}) ->
    {N,A+1,T}.

joe() ->    
     {"Joe", 21, "999-999"}.

showPerson({N,A,T}) ->
    io:format("name: ~p  age: ~p  phone: ~p~n", [N, A, T]).

test1() ->
    showPerson(joe()).

test2() ->
    showPerson(birthday(joe())).

