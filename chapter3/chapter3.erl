%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

%% Chapter 3

-module(chapter3).

-export([convert/1,listlen/1,listlen2/1]).
-export([index/1,index/2,index2/2,index3/2]).
-export([f/1,g/1,safe/1,preferred/1]).

-export([factorial/1,guard/2,guard2/2]).
-export([even/1, number/1]).
-export([bump/1,average/1,sum/1,len/1]).
-export([member/2]).
-export([sum_acc/2,sum2/1,bump_acc/2,bump2/1]).
-export([reverse/1,reverse_acc/2]).
-export([merge/2,mergeL/3,mergeR/3]).
-export([average_acc/3,average2/1]).
-export([sum_acc/3,sum3/1]).

-export([test1/1,test2/1]).

-export([return_error/1,try_return/1,try_wildcard/1,try_return2/1,return/1]).

%% Conditional Evaluations

% To convert an atom representing a day into an integer.

convert(Day) ->
  case Day of
    monday    -> 1;
    tuesday   -> 2;
    wednesday -> 3;
    thursday  -> 4;
    friday    -> 5;
    saturday  -> 6;
    sunday    -> 7;
    Other     -> {error, unknown_day}
  end.

% Calculating the length of a list.

listlen([])     -> 0;
listlen([_|Xs]) -> 1 + listlen(Xs).

% Rewrite this directly using a case expression:

listlen2(Y) ->
  case Y of
    []     -> 0;
    [_|Xs] -> 1 + listlen2(Xs)
  end.

% Indexing into a list, i.e. looking for the nth element of a  
% list (with numbering from zero).

index(0,[X|_])           -> X;
index(N,[_|Xs]) when N>0 -> index(N-1,Xs).

% Defining an index function using a case expression.

index2(X,Y) ->
   index({X,Y}).

index(Z) ->
  case Z of
    {0,[X|_]}            -> X;
    {N,[_|Xs]} when N>0  -> index2(N-1,Xs)
  end.

% Defining an index function using nested case expressions.

index3(X,Y) ->
  case X of
    0 ->
      case Y of
        [Z|_]    -> Z
      end;
    N when N>0 ->
      case Y of
        [_|Zs]   -> index3(N-1,Zs)
      end
  end.

% Examples to illustrate the scope of variables.

f(X)      -> Y=X+1,Y*X.

g([0|Xs]) -> g(Xs);
g([Y|Xs]) -> Y+g(Xs);
g([])     -> 0.

% Binding a variable in both arms of a case expression is
% possible ...

safe(X) ->                
  case X of                  
    one -> Y = 12;               
    _   -> Y = 196               
  end,                         
  X+Y.                        

% ... but the preferred style is to assign a value to the variable.
% where the value is defined using a case

preferred(X) ->
  Y = case X of
        one -> 12;
        _   -> 196
      end,       
  X+Y.           

%% Guards 

% Defining factorial using a guard

factorial(N) when N > 0 ->
  N * factorial(N - 1);
factorial(0) -> 1.

% An example of a complex guard ...

guard(X,Y) when not(((X>Y) or not(is_atom(X)) ) and (is_atom(Y) or (X==3.4))) ->
  X+Y.

% ... and the same guard rewritten using ; and ,

guard2(X,Y) when not(X>Y) , is_atom(X) ; not(is_atom(Y)) , X=/=3.4 ->
  X+Y.

% Examples from the examples.erl module.

even(Int) when Int rem 2 == 0 -> true;
even(Int) when Int rem 2 == 1 -> false.

number(Num) when is_integer(Num) -> integer;
number(Num) when is_float(Num)   -> float;
number(_Other)                   -> false.

%% Recursion

% The bump function: add one to each element of a list.
% A first example of recursion.

bump([]) -> [];
bump([Head | Tail]) -> [Head + 1 | bump(Tail)].

% Finding the average value in a numeric list.

average(List) -> sum(List) / len(List).

sum([]) -> 0;
sum([Head | Tail]) -> Head + sum(Tail).

len([]) -> 0;
len([_ | Tail]) -> 1 + len(Tail).

% Is the first argument a memeer of the second argument (a list)?

member(_, [])      -> false;
member(H, [H | _]) -> true;
member(H, [_ | T]) -> member(H, T).

% Summing a list using tail recursion. 

sum_acc([],Sum) -> Sum;
sum_acc([Head|Tail], Sum) -> sum_acc(Tail, Head+Sum).

sum2(List) -> sum_acc(List,0).

% Bumping every element in a list using an accumulator.

bump2(List) -> bump_acc(List, []).

bump_acc([], Acc)            -> reverse(Acc);
bump_acc([Head | Tail], Acc) -> bump_acc(Tail, [Head + 1 | Acc]).

% Reversing a list.

reverse(List) -> reverse_acc(List, []).

reverse_acc([], Acc) -> Acc;
reverse_acc([H | T], Acc) -> reverse_acc(T, [H | Acc]).

% Merging the elements of two lists.

merge(Xs,Ys) ->
 lists:reverse(mergeL(Xs,Ys,[])).

mergeL([X|Xs],Ys,Zs) ->
  mergeR(Xs,Ys,[X|Zs]);
mergeL([],[],Zs) ->
  Zs.

mergeR(Xs,[Y|Ys],Zs) ->
  mergeL(Xs,Ys,[Y|Zs]);
mergeR([],[],Zs) ->
  Zs.

% Average revisited, this time using two accumulators.

average2(List) -> average_acc(List, 0,0).

average_acc([], Sum, Length) ->
  Sum / Length;
average_acc([H | T], Sum, Length) ->
  average_acc(T, Sum + H, Length + 1).

% Iterative version of sum

sum3(Boundary) -> sum_acc(1, Boundary, 0).

sum_acc(Index, Boundary, Sum) when Index =< Boundary ->
  sum_acc(Index + 1, Boundary, Sum + Index);
sum_acc(_I, _B, Sum)->
   Sum.

%% Runtime Errors

% Fault-provoking functions.

test1(N) ->
  case N of
    -1 -> false;
    1  -> true
  end.

test2(N) ->
  if
    N < 0 -> false;
    N > 0 -> true
  end.

% 

%% Handling Errors

% Examples showing different kinds of error behaviour.

try_return(X) when is_integer(X) -> 
  try return_error(X) of 
    Val -> {normal, Val} 
  catch 
    exit:Reason -> {exit, Reason}; 
    throw:Throw -> {throw, Throw}; 
    error:Error -> {error, Error} 
  end. 

try_wildcard(X) when is_integer(X) ->
  try return_error(X)
    catch
    throw:Throw -> {throw, Throw};
    error:_     -> error;
    Type:Error  -> {Type, Error};
    _           -> other;                 %% Will never be returned
    _:_         -> other                  %% Will never be returned
  end.

try_return2(X) when is_integer(X) ->
  try return_error(X) of
    Val -> {normal, Val}
    catch
    exit:_  -> 34;
    throw:_ -> 99;
    error:_ -> 678
  end.

return_error(X) when X < 0 ->
  throw({'EXIT', {badarith,
                 [{exception,return_error,1},
                  {erl_eval,do_apply,5},
                  {shell,exprs,6},
                  {shell,eval_exprs,6},
                  {shell,eval_loop,3}]}});
return_error(X) when X == 0 ->
  1/X;
return_error(X) when X > 0->
  {'EXIT', {badarith, [{exception,return_error,1},
                       {erl_eval,do_apply,5},
                       {shell,exprs,6},
                       {shell,eval_exprs,6},
                       {shell,eval_loop,3}]}}.

return(X) when is_integer(X) ->
  catch return_error(X).

%% Exercise 3-10: sample text

%% Write a function that will print this in a readable form,
%% so that duplicates are removed and adjacent numbers are put into a
%% range. You might like to think of doing this via a function which turns
%% the earlier list of occurrences into a list like
%% [{1,2},{4,6},{98,98},{100,100},{102,102}]
%% through a sequence of transformations.
