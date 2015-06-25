%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/

%% Demo module from Chapter 2

-module(demo).
-export([double/1]).

% This is a comment.
% Everything on a line after % is ignored.

double(Value) ->
  times(Value, 2).
times(X,Y) ->
  X*Y.

%% Note: times/2 is deliberately omitteed from the export list.
