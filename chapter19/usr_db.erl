%% @author Francesco Cesarini <francesco@erlang-consulting.com>
%% @author Simon Thompson [http://www.cs.kent.ac.uk/~sjt/]
%% @doc Back end for the mobile subscriber database. The module provides an example of using ETS and DETS tables.
%% @reference <a href="http://oreilly.com/catalog/9780596518189/">Erlang Programming</a>,
%%  <em> Francesco Cesarini and Simon Thompson</em>,
%%  O'Reilly, 2009.
%% @copyright 2009 Francesco Cesarini and Simon Thompson




-module(usr_db).
-export([create_tables/1, close_tables/0, add_usr/1, update_usr/1,
	 delete_usr/2,  delete_usr/1]). %lookup_id/1, lookup_msisdn/1, get_index/1,
	 %delete_disabled/0, delete_usr/1, restore_backup/0]).%,
	 %testFun/0,testFun2/0,testFun3/0]).

-include("usr.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Create the ETS and DETS tables which implement the database. The  
%% argument gives the filename which is used to hold the DETS table. 
%% If the table can be created, an `ok' tuple containing a
%% reference to the created table is returned; if not, it returns an `error' 
%% tuple with a string describing the error. 

%% @spec create_tables(string()) -> {ok, reference()} | {error, string()}

-spec(create_tables(string()) -> {ok, ref()} | {error, string()}).

create_tables(FileName) ->
    ets:new(subRam, [named_table, {keypos, #usr.msisdn}]),
    ets:new(subIndex, [named_table]),
    dets:open_file(subDisk, [{file, FileName}, {keypos, #usr.msisdn}]).

%% @doc Close the ETS and DETS tables implementing the database. 
%% Returns either `ok' or and `error'
%% tuple with the reason for the failure to close the DETS table.

%% @spec close_tables() -> ok | {error, string()}

-spec(close_tables() -> ok | {error, string()}).

close_tables() ->
    ets:delete(subRam),
    ets:delete(subIndex),
    dets:close(subDisk).

%% @doc Add a user (of the `usr' record type) to the database.

%% @spec add_usr(#usr{}) -> ok

-spec(add_usr(#usr{}) -> integer()).

add_usr(#usr{msisdn=PhoneNo, id=CustId} = Usr) ->
    ets:insert(subIndex, {CustId, PhoneNo}),
    update_usr(Usr).

%% @doc Updates the ram and disk tables with a `Usr'.
 
%% @spec update_usr(#usr{}) -> ok

-spec(update_usr(#usr{}) -> ok).

update_usr(Usr) ->
    ets:insert(subRam, Usr),
    dets:insert(subDisk, Usr),
    ok.    

%% @doc Delete a user, specified by their customer id. Returns
%% either `ok' or an `error' tuple with a reason, if either the 
%% lookup of the id fails, or the delete of the tuple.

%% @spec delete_usr(integer()) -> ok|{error,string()}

-spec(delete_usr(integer()) -> ok|{error,string()}).

delete_usr(CustId) ->
    case get_index(CustId) of
	{ok,PhoneNo} ->
	    delete_usr(PhoneNo, CustId);
	{error, instance} ->
	    {error, instance}
    end.

%% @doc Delete a user, specified by their phone number and customer id. Returns
%% either `ok' or an `error' tuple with a reason.

%% @spec delete_usr(integer(),integer()) -> ok|{error,string()}

-spec(delete_usr(integer(),integer()) -> ok|{error,string()}).

delete_usr(PhoneNo, CustId) ->
    dets:delete(subDisk, PhoneNo),
    ets:delete(subRam, PhoneNo),
    ets:delete(subIndex, CustId),
    ok.

%% @doc Lookup the `usr' record for a customer, specified by a `CustId'.
%% If successful return an `ok' tuple with the record; otherwise
%% return an `error' tuple.

%% @spec lookup_id(integer()) -> {ok, #usr{}}|{error, instance}

-spec(lookup_id(integer()) -> {ok, #usr{}}|{error, instance}).

lookup_id(CustId) ->
    case get_index(CustId) of
	{ok,PhoneNo} ->
	    lookup_msisdn(PhoneNo);
	{error, instance} ->
	    {error, instance}
    end.

%% @doc Lookup the `usr' record for a customer, specified by a `PhoneNo'.
%% If successful return an `ok' tuple with the record; otherwise
%% return an `error' tuple.

%% @spec lookup_msisdn(integer()) -> {ok, #usr{}}|{error, instance}

-spec(lookup_msisdn(integer()) -> {ok, #usr{}}|{error, instance}).

lookup_msisdn(PhoneNo) ->
    case ets:lookup(subRam, PhoneNo) of
	[Usr] ->
	    {ok, Usr};
	[] ->
	    {error, instance}
    end.

%% @doc Lookup the phone number for a customer, specified by a `CustId'.
%% If successful return an `ok' tuple with the number; otherwise
%% return an `error' tuple.

%% @spec get_index(integer()) -> {ok, integer()}|{error, instance}

-spec(get_index(integer()) -> {ok, integer()}|{error, instance}).

get_index(CustId) ->
    case ets:lookup(subIndex, CustId) of
	[{CustId,PhoneNo}] -> {ok, PhoneNo};
	[]                 -> {error, instance}
    end.

%% @doc Delete subscribers with a `disabled' status field.

%% @spec delete_disabled() -> ok

-spec(delete_disabled() -> ok).

delete_disabled() ->
    ets:safefixtable(subRam, true),
    catch loop_delete_disabled(ets:first(subRam)),
    ets:safefixtable(subRam, false),
    ok.

%% Not exported; local to delete_disabled.

loop_delete_disabled('$end_of_table') ->
    ok;
loop_delete_disabled(PhoneNo) ->
    case ets:lookup(subRam, PhoneNo) of
	[#usr{status=disabled, id = CustId}] ->
	    delete_usr(PhoneNo, CustId);
	_ ->
	    ok
    end,
    loop_delete_disabled(ets:next(subRam, PhoneNo)).


%% @doc Restore the ETS tables from the DETS table.

%% @spec restore_backup() -> true

-spec(restore_backup() -> true).

restore_backup() ->
    Insert = fun(#usr{msisdn=PhoneNo, id=Id} = Usr) ->
		     ets:insert(subRam, Usr),
		     ets:insert(subIndex, {Id, PhoneNo}),
		     continue
	     end,
    dets:traverse(subDisk, Insert).


%%% TESTING

testFun() ->
     eunit:test(
      {spawn, 
      {setup,
       fun ()  -> create_tables("UsrTabFile") end,                % setup
       fun (_) -> ?cmd("rm UsrTabFile") end,                      % cleanup
       ?_assertMatch({error,instance}, lookup_id(1)) }}).

testFun2() ->
     eunit:test(
      {spawn, 
      {setup,
       fun ()  -> create_tables("UsrTabFile"),
		  Seq = lists:seq(1,100000),
		  Add = fun(Id) -> add_usr(#usr{msisdn = 700000000 + Id, 
                                        id = Id, 
                                        plan = prepay, 
                                        services = [data, sms, lbs]}) 
			end,
		  lists:foreach(Add, Seq)
       end,
       fun (_) -> ?cmd("rm UsrTabFile") end, 
       ?_assertMatch({ok, #usr{status = enabled}} , lookup_msisdn(700000001) ) }}).

testFun3() ->
     eunit:test(
      {spawn, 
      {setup,
       fun ()  -> create_tables("UsrTabFile"),
		  Seq = lists:seq(1,100000),
		  Add = fun(Id) -> add_usr(#usr{msisdn = 700000000 + Id, 
                                        id = Id, 
                                        plan = prepay, 
                                        services = [data, sms, lbs]}) 
			end,
		  lists:foreach(Add, Seq),
		  {ok, UsrRec} = usr_db:lookup_msisdn(700000001),
		  update_usr(UsrRec#usr{services = [data, sms], status = disabled})
       end,
       fun (_) -> ?cmd("rm UsrTabFile") end, 
       ?_assertMatch({ok, #usr{status = disabled}} , lookup_msisdn(700000001) ) }}).
       



