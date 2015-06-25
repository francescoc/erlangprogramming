%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(index2).

-export([processFile/2,test1/0,test2/0,test3/0,accumulate/1]).

-define(Punctuation,"(\\ |\\,|\\.|\\;|\\:|\\t|\\n|\\(|\\))+").

% Processing text: helper functions.
% Note how the "\" in the regexp string have themselves
% to be quoted.

% Variant of text.erl, replacing list construction with 
% side-effects on an ETS.

% It's aparent from this that keys can be anything, and not just 
% atoms. Also worth noting that any any position can be the key position.
% Pass this in as information when the ETS table is created.

% Convert a text file to an ETS table, with punctuation stripped.
% Short words (less than three characters) are stripped too, and
% words converted to lower case.

% Shows the difference between set, bag, dupicate_bag with the
% example file Text.txt. The size is
% set               
					 

processFile(File,Table) ->
    {ok,IoDevice} = file:open(File,[read]),
    processLines(IoDevice,Table,1).

processLines(IoDevice,Table,N) ->
    case io:get_line(IoDevice,"") of
	eof ->
	    ok;
	Line -> 
	    processLine(Line,Table,N),
	    processLines(IoDevice,Table,N+1)
    end.

processLine(Line,Table,N) ->
    case regexp:split(Line,?Punctuation) of
	{ok,Words} ->
	    processWords(Words,Table,N) ;
	_ -> []
    end.

processWords(Words,Table,N) ->
    case Words of
	[] -> ok;
	[Word|Rest] ->
	    if length(Word) > 3 ->
		    Normalise = Word, %string:to_lower(Word),
		    ets:insert(Table,{{ Normalise , N}});
	       true -> ok
	    end,
	    processWords(Rest,Table,N)
    end.

index(File) ->
    Table = ets:new(myTable, [bag]),
    processFile(File,Table),
    showIndex(Table).

showIndex(TabId) ->
    case ets:first(TabId) of
	'$end_of_table' ->
	    ok;
	First  ->
	    showEntry(TabId,First),
	    showIndexNext(TabId,First)
    end.

showIndexNext(TabId,Entry) ->
    case ets:next(TabId,Entry) of
	'$end_of_table' ->
	    ok;
	Next  ->
	    showEntry(TabId,Next),
	    showIndexNext(TabId,Next)
    end.

showEntry(TabId,Entry) ->    
    io:format("~p ~n",[ets:lookup(TabId,Entry)]).

prettyIndex(TabId) ->
    case ets:first(TabId) of
	'$end_of_table' ->
	    ok;
	First  ->
	    case First of
		{Word, N} -> 
		    IndexEntry = {Word, [N]}
	    end,
	    prettyIndexNext(TabId,First,IndexEntry)
    end.

prettyIndexNext(TabId,Entry,{Word, Lines}=IndexEntry) ->
    Next = ets:next(TabId,Entry),
    case Next of
	'$end_of_table' ->
	    prettyEntry(IndexEntry);
	{NextWord, M}  ->
	    if 
		NextWord == Word ->
		    prettyIndexNext(TabId,Next,{Word, [M|Lines]});
		true ->
		    prettyEntry(IndexEntry),
		    prettyIndexNext(TabId,Next,{NextWord, [M]})
	    end
    end.

prettyEntry({Word, Lines}) ->    
    io:put_chars(pad(15,Word)),
    prettyList(accumulate(Lines)).

pad(N,Word) ->
    Len = length(Word),
    if 
	Len>=N ->
	    Word;
	true ->
	    Word ++ replicate(N-Len, " ")
    end.

replicate(0,_) ->
     [];
replicate(N,X) ->
    X ++ replicate(N-1,X).

prettyList([]) ->
    ok;
prettyList([{N}]) ->
    io:format("~p.~n",[N]);
prettyList([{N,M}]) ->
    io:format("~p-~p.~n",[N,M]);
prettyList([{N}|Ns]) ->
    io:format("~p,",[N]),
    prettyList(Ns);
prettyList([{N,M}|Ns]) ->
    io:format("~p-~p,",[N,M]),
    prettyList(Ns).

accumulate(Ns) ->
    accumulate(Ns,[]).

accumulate([],L) -> L;
accumulate([N|Ns],[]) ->
    accumulate(Ns,[{N}]);
accumulate([N|Ns],[{P}|Rest]=Ms) ->
    if
	N==P ->
	    accumulate(Ns,[{P}|Rest]);
	N+1==P ->
	    accumulate(Ns,[{N,P}|Rest]);
	true ->
	    accumulate(Ns,[{N}|Ms])
    end;
accumulate([N|Ns],[{P,Q}|Rest]=Ms) ->
    if
	N==P ->
	    accumulate(Ns,[{P,Q}|Rest]);
	N+1==P ->
	    accumulate(Ns,[{N,Q}|Rest]);
	true ->
	    accumulate(Ns,[{N}|Ms])
    end.    


% test code

test1() ->
    TabId = ets:new(myTable, [set,{keypos,2}]),
    processFile("Text.txt",TabId),
    First = ets:first(TabId),
    io:format("First element is: ~p~n",[First]),
    Next = ets:next(TabId,First),
    io:format("The next element is: ~p~n",[Next]),
    NextBut1 = ets:next(TabId,Next),
    io:format("And the next but one is: ~p~n",[NextBut1]),
    ets:tab2list(TabId),
    ets:info(TabId).
    
test2() ->
    index("Short.txt").

test3() ->
    TabId = ets:new(myTable, [ordered_set]),
    processFile("Short.txt",TabId),
    prettyIndex(TabId).    
    



    
