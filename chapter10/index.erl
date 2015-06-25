%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(index).

-export([index/1,processFile/1,prettyEntry/1]). 

index(File) ->
  ets:new(indexTable, [ordered_set, named_table]),
  processFile(File),
  prettyIndex().

processFile(File) ->
  {ok,IoDevice} = file:open(File,[read]),
  processLines(IoDevice,1).

processLines(IoDevice,N) ->
  case io:get_line(IoDevice,"") of
    eof ->
      ok;
    Line ->
      processLine(Line,N),
      processLines(IoDevice,N+1)
  end.
					 
-define(Punctuation,"(\\ |\\,|\\.|\\;|\\:|\\t|\\n|\\(|\\))+").

processLine(Line,N) ->
  case regexp:split(Line,?Punctuation) of
    {ok,Words} ->
      processWords(Words,N) ;
    _ -> []
  end.

processWords(Words,N) ->
  case Words of
    [] -> ok;
    [Word|Rest] ->
      if
        length(Word) > 3 ->
          Normalise = string:to_lower(Word),
          ets:insert(indexTable,{{Normalise , N}});
        true -> ok
      end,
      processWords(Rest,N)
  end.

prettyIndex() ->
  case ets:first(indexTable) of
    '$end_of_table' ->
      ok;
    First  ->
      case First of
        {Word, N} ->
          IndexEntry = {Word, [N]}
      end,
      prettyIndexNext(First,IndexEntry)
  end.


prettyIndexNext(Entry,{Word, Lines}=IndexEntry) ->
  Next = ets:next(indexTable,Entry),
  case Next of
    '$end_of_table' ->
      prettyEntry(IndexEntry);
    {NextWord, M}  ->
      if
        NextWord == Word ->
          prettyIndexNext(Next,{Word, [M|Lines]});
        true ->
          prettyEntry(IndexEntry),
          prettyIndexNext(Next,{NextWord, [M]})
      end
  end.

prettyEntry(IndexEntry) ->
    % exercise
    ok.
    




    
