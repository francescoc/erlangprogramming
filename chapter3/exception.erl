%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(exception).
-export([return_error/1,try_return/1,try_wildcard/1,try_return2/1,return/1]).

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

return(X) when is_integer(X) ->
  catch return_error(X).

