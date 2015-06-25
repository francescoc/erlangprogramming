%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

%%% File    : usr_db.erl
%%% Description : Database API for subscriber DB

-module(usr_db).
-include("usr.hrl").
-export([create_tables/1, close_tables/0, add_usr/1, update_usr/1, delete_usr/1,
         lookup_id/1, lookup_msisdn/1, restore_backup/0, delete_disabled/0]).

create_tables(FileName) ->
  ets:new(usrRam, [named_table, {keypos, #usr.msisdn}]),
  ets:new(usrIndex, [named_table]),
  dets:open_file(usrDisk, [{file, FileName}, {keypos, #usr.msisdn}]).

close_tables() ->
  ets:delete(usrRam),
  ets:delete(usrIndex),
  dets:close(usrDisk).

add_usr(#usr{msisdn=PhoneNo, id=CustId} = Usr) ->
    ets:insert(usrIndex, {CustId, PhoneNo}),
    update_usr(Usr).

update_usr(Usr) ->
    ets:insert(usrRam, Usr),
    dets:insert(usrDisk, Usr),
    ok.

lookup_id(CustId) ->
    case get_index(CustId) of
      {ok,PhoneNo}      -> lookup_msisdn(PhoneNo);
      {error, instance} -> {error, instance}
    end.

%% NB this code omitted from the description in Chapter 10.

%% Delete a user, specified by their customer id. Returns
%% either `ok' or an `error' tuple with a reason, if either the 
%% lookup of the id fails, or the delete of the tuple.


delete_usr(CustId) ->
    case get_index(CustId) of
	{ok,PhoneNo} ->
	    delete_usr(PhoneNo, CustId);
	{error, instance} ->
	    {error, instance}
    end.

%% Delete a user, specified by their phone number and customer id. Returns
%% either `ok' or an `error' tuple with a reason.

delete_usr(PhoneNo, CustId) ->
    dets:delete(subDisk, PhoneNo),
    ets:delete(subRam, PhoneNo),
    ets:delete(subIndex, CustId),
    ok.


lookup_msisdn(PhoneNo) ->
    case ets:lookup(usrRam, PhoneNo) of
      [Usr] -> {ok, Usr};
      []    -> {error, instance}
    end.

get_index(CustId) ->
    case ets:lookup(usrIndex, CustId) of
      [{CustId,PhoneNo}] -> {ok, PhoneNo};
      []                 -> {error, instance}
    end.

restore_backup() ->
    Insert = fun(#usr{msisdn=PhoneNo, id=Id} = Usr) ->
               ets:insert(usrRam, Usr),
               ets:insert(usrIndex, {Id, PhoneNo}),
               continue
             end,
    dets:traverse(usrDisk, Insert).

delete_disabled() ->
    ets:safe_fixtable(usrRam, true),
    catch loop_delete_disabled(ets:first(usrRam)),
    ets:safe_fixtable(usrRam, false),
    ok.

loop_delete_disabled('$end_of_table') ->
    ok;
loop_delete_disabled(PhoneNo) ->
    case ets:lookup(usrRam, PhoneNo) of
      [#usr{status=disabled, id = CustId}] ->
        delete_usr(PhoneNo, CustId);
      _ ->
        ok
    end,
    loop_delete_disabled(ets:next(usrRam, PhoneNo)).
