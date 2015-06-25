%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(tcp).
-export([decode/1,seg1/0,seg2/0]).

decode(Segment) ->
    case Segment of 
	<< SourcePort:16, DestinationPort:16,
	   SequenceNumber:32,
	   AckNumber:32,
	   DataOffset:4, _Reserved:4, Flags:8, WindowSize:16,
	   Checksum:16, UrgentPointer:16,
	   Payload/binary>> when DataOffset>4
	->
	    OptSize = (DataOffset - 5)*32,
	    << Options:OptSize, Message/binary >> = Payload,
	    <<CWR:1, ECE:1, URG:1, ACK:1, PSH:1, RST:1, SYN:1, FIN:1>> = <<Flags:8>>,
 	    %% Can now process the Message according to the
	    %% Options (if any) and the flags CWR, ..., FIN. 
	    binary_to_list(Message)
    end.

seg1() -> << 0:16, 0:16,
	    0:32,
	    0:32,
	    5:4, 0:4, 0:8, 0:16,
	    0:16, 0:16,
	    "message">>.

seg2() -> << 0:16, 0:16,
	    0:32,
	    0:32,
	    7:4, 0:4, 0:8, 0:16,
	    0:16, 0:16,
	    0:64,
	    "message">>.
