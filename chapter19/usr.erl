%% @author Francseco Cesarini <francesco@erlang-consulting.com>
%% @author Simon Thompson <s.j.thompson@kent.ac.uk>
%% @doc  API and server code for the  mobile subscriber database. The functions exported 
%% by the module fall into three classes:
%% === System start and stop ===
%% {@link start/0. `start/0'}, 
%% {@link start/1. `start/1'}, 
%% {@link stop/0. `stop/0'} 
%% === Customer Service API ===
%% {@link add_usr/3. `add_usr/3'},
%% {@link delete_usr/1. `delete_usr/1'},
%% {@link set_service/3. `set_service/3'},
%% {@link set_status/2. `set_status/2'},
%% {@link delete_disabled/0. `delete_disabled/0'},
%% {@link lookup_id/1. `lookup_id/1'}
%% === Service API ===
%% {@link lookup_msisdn/1. `lookup_msisdn/1'},
%% {@link service_flag/2. `service_flag/2'}


%% @reference Erlang Programming,
%%  <em> Francseco Cesarini and Simon Thompson</em>,
%%  O'Reilly, 2009.
%% @copyright 2009 Francseco Cesarini and Simon Thompson

%% @headerfile "usr.hrl"

%% Type definitions and comments for edoc.

%% @type instTime() = instance | timeout. Two different errors.



-module(usr).
-export([start/0, start/1, stop/0, init/2]).
-export([add_usr/3, delete_usr/1, set_service/3, set_status/2, 
	 delete_disabled/0, lookup_id/1]).
-export([lookup_msisdn/1, service_flag/2]).

-include("usr.hrl").

-define(TIMEOUT, 10000).

%% Type definitions for typer.

-type(instTime() :: instance | timeout).



%%----------------------------------------------
%% Exported Client Functions
%% Starting and Stopping the System
%%----------------------------------------------

%% @doc Start the system with the default filename.
%% Returns `ok' or an `error' tuple.

%% @spec start() -> ok | {error, starting}

-spec(start() -> ok | {error, starting}).

start() ->
    start("usrDb").

%% @doc Start the system with the given `Filename'.
%% Returns `ok' or an `error' tuple.

%% @spec start(string()) -> ok | {error, starting}

-spec(start(string()) -> ok | {error, starting}).

start(FileName) ->
    register(?MODULE, spawn(?MODULE, init, [FileName, self()])),
    receive started-> ok after ?TIMEOUT -> {error, starting} end.

%% @doc Stop the system.
%% Returns `ok' or an `error' tuple, containing the reason for failure.

%% @spec stop() -> ok | {error, string()}

-spec(stop() -> ok | {error, string()}).

stop() ->
    call(stop).

%%----------------------------------------------
%% Customer Service API
%%----------------------------------------------

%% @doc Add a user with specified phone number, customer id and plan type.
%% Returns `ok' or an `error' tuple.

%% @spec add_usr(integer(), integer(), plan()) -> ok | {error, instTime()}

-spec(add_usr(integer(), integer(), plan()) -> ok | {error, instTime()}).

add_usr(PhoneNum, CustId, Plan) ->
    call({add_usr, PhoneNum, CustId, Plan}).

%% @doc Delete a user and all their associated entries.

delete_usr(CustId) ->    
    call({delete_usr, CustId}).

%% @doc Set or unset a particular service for a particular customer. A `true' Flag
%% sets the service and `false' unsets it.

%% @spec set_service(integer(), service(), bool()) -> ok | {error, instTime()}

-spec(set_service(integer(), service(), bool()) -> ok | {error, instTime()}).

set_service(CustId, Service, Flag) ->
    call({set_service, CustId, Service, Flag}).

%% @doc Set the status of a customer to be enabled or disabled.

%% @spec set_status(integer(), status()) -> ok | {error, instTime()}

-spec(set_status(integer(), status()) -> ok | {error, instTime()}).  

set_status(CustId, Status) ->
    call({set_status, CustId, Status}).

%% @doc Traverses the table and deletes all users whose status is set to `disabled'. 

%% @spec delete_disabled()-> ok | {error, timeout}

-spec(delete_disabled()-> ok | {error, timeout}).

delete_disabled() ->
    call(delete_disabled).

%% @doc Lookup a user based on their CustId and return a record of type `usr'. If the
%% customer does not exist, an `error' tuple is returned.

%% @spec lookup_id(integer()) -> {ok, #usr{}} | {error, instance}

-spec(lookup_id(integer()) -> {ok, #usr{}} | {error, instance}).

lookup_id(CustId) ->
    usr_db:lookup_id(CustId).

%%----------------------------------------------
%% Service API
%%----------------------------------------------

%% @doc Lookup the `usr' record for a customer, specified by a `PhoneNo'.
%% If successful return an `ok' tuple with the record; otherwise
%% return an `error' tuple.

%% @spec lookup_msisdn(integer()) -> {ok, #usr{}}|{error, instance}

-spec(lookup_msisdn(integer()) -> {ok, #usr{}}|{error, instance}).

lookup_msisdn(PhoneNo) ->
    usr_db:lookup_msisdn(PhoneNo).

%% @doc This function will check if a user exists and has an enabled status. If so, it
%%  will traverse the list of services to determine if the subscriber is allowed to use 
%% this Service in a particular request. This is a variant of the lookup_msisdn/1 
%% function, where logical checks are done in this module.

%% @spec service_flag(integer(), service()) -> bool() | {error, instance | disabled}

-spec(service_flag(integer(), service()) -> bool() | {error, instance | disabled}). 

service_flag(PhoneNo, Service) ->
    case usr_db:lookup_msisdn(PhoneNo) of
	{ok,#usr{services=Services, status=enabled}} ->
	    lists:member(Service, Services);
	{ok, #usr{status=disabled}} ->
	    {error, disabled};
	{error, Reason} ->
	    {error, Reason}
    end.


call(Request) ->
    Ref = make_ref(),
    ?MODULE! {request, {self(), Ref}, Request},
    receive 
	{reply, Ref, Reply} -> Reply 
    after 
	?TIMEOUT -> {error, timeout}
    end.

reply({From, Ref}, Reply) ->
    From ! {reply, Ref, Reply}.

%% @hidden Because it should not be part of the external interface.

init(FileName, Pid) ->
    usr_db:create_tables(FileName),
    usr_db:restore_backup(),
    Pid ! started,
    loop().

loop() ->
    receive
	{request, From, stop} ->
	    reply(From, usr_db:close_tables());
	{request, From, Request} ->
	    Reply = request(Request),
	    reply(From, Reply),
	    loop()
    end.


request({add_usr, PhoneNo, CustId, Plan}) ->
    usr_db:add_usr(#usr{msisdn=PhoneNo, 
			   id=CustId, 
			   plan=Plan});

request({delete_usr, CustId}) ->
    usr_db:delete_usr(CustId);

request({set_service, CustId, Service, Flag}) ->
    case usr_db:lookup_id(CustId) of
	{ok, Usr} ->
	    Services = lists:delete(Service, Usr#usr.services),
	    NewServices = case Flag of
			      true  -> [Service|Services];
			      false -> Services
			  end,
	    usr_db:update_usr(Usr#usr{services=NewServices});
	[] ->
	    {error, instance}
    end;


request(delete_disabled) ->
    usr_db:delete_disabled().



