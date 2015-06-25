%%% File        : usr.hrl
%%% Description : Include file for cell user db

%% @type plan() = prepay|postpay. The two payment types for mobile subscribers.
%% @type status() = enabled | disabled. The status of a customer can be enabled 
%% or disabled. 
%% @type service() = atom(). Services are specified by atoms, including
%% (but not limited to) `data', `lbs' and `sms'. `Data' confirms the user 
%% has subscribed to a data plan, `sms' allows the user to send and receive 
%% premium rated smses, while `lbs' would allow third parties to execute 
%% location lookups on this particular user. 

-type(plan()     :: prepay|postpay).
-type(status()   :: enabled | disabled). 
-type(service()  :: atom()).

%% @type usr() = #usr{msisdn   = integer(),
%%	              id       = integer(),
%%	              status   = status(), 
%%	              plan     = plan(), 
%%	              services = [service()]}.
%% Record for users. The default value of `status' is `enabled' and of `services' is `[]'.



-record(usr, {msisdn             ::integer(),
	       id                ::integer(),
	       status = enabled  ::status(),
	       plan              ::plan(), 
	       services = []     ::[service()]
}). 
