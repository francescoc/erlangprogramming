-module(echoFac). 
-export([test/0]). 

test() -> 
  Cmd = "ruby echoFac.rb", 
  Port = open_port({spawn, Cmd}, [{packet, 4}, use_stdio, exit_status, binary]),
  Payload = term_to_binary({fac, list_to_binary(integer_to_list(23))}), 
  port_command(Port, Payload), 
  receive 
    {Port, {data, Data}} -> 
      {result, Text} = binary_to_term(Data), 
      Blah = binary_to_list(Text),
      io:format("~p~n", [Blah]) 
  end.
