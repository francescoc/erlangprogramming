%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(hof1).

-export([double/1,double2/1,double3/1,rev/1,map/2,reverse/1,evens/1,palins/1,filter/2,
         addTwo/0,showList/1,doAll/2,times/1,sendTo/1,palin/1]).

double([]) ->
      [];
double([X|Xs]) ->
     [X*2 | double(Xs)].

double2(Xs) ->
     map(fun(X) -> X*2 end, Xs).

rev([]) ->
    [];
rev([X|Xs]) ->
    [reverse(X) | rev(Xs)].

reverse(Ys) ->
    reverse(Ys,[]).

reverse([],Zs) ->
    Zs;
reverse([X|Xs],Zs) ->
    reverse(Xs,[X|Zs]).

map(F,[]) ->
    [];
map(F,[X|Xs]) ->
    [ F(X) | map(F,Xs) ].


evens([]) ->
    [];
evens([X|Xs]) ->
    case X rem 2 == 0 of
	true ->
	    [X| evens(Xs)];
	_ ->
	    evens(Xs)
    end.

palin(X) ->
   X == reverse(X).

palins([]) -> 
     [];
palins([X|Xs]) ->
     case palin(X) of
 	true ->
 	    [X| palins(Xs)];
 	_ ->
 	    palins(Xs)
     end.

palins2(Xs) ->
    filter( fun(X) -> X == reverse(X) end, Xs).

filter(P,[]) -> 
    [];
filter(P,[X|Xs]) ->
    case P(X) of
	true ->
	    [X| filter(P,Xs)];
	_ ->
	    filter(P,Xs)
    end.

addTwo() ->
    fun (X,Y) ->
	    X+Y end.

myHead () ->
    fun ( [], Y ) ->
		 Y;
	( [X|_], Y ) ->
		  X
     end.

showList([]) ->
    ok;
showList([X|Xs]) ->
    io:format("Element:~p ~n",[X]),
    showList(Xs).


doAll(F,[]) ->
    ok;
doAll(F,[X|Xs]) ->
    F(X),
    doAll(F,Xs).

times(X) ->
    fun (Y) ->
	     X*Y end.

double3(Xs) ->
    map(times(2),Xs).

sendTo(Pid) ->
    fun (X) ->
	     Pid ! X 
    end.
