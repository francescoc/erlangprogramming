%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

{application, usr,
 [{description, "Mobile Services Database"},
  {vsn, "1.0"},
  {modules, [usr, usr_db, usr_sup, usr_app]},
  {registered, [usr, usr_sup]},
  {applications, [kernel, stdlib]},
  {env, [{dets_name, "usrDb"}]},
  {mod, {usr_app,[]}}]}.
