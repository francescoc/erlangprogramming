%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

%%% File    : usr.hrl
%%% Description : Include file for  user db

-record(usr, {msisdn,             %int()
              id,                 %term()
              status = enabled,   %atom(), enabled | disabled
              plan,               %atom(), prepay | postpay
              services = []}).    %[atom()], service flag list
