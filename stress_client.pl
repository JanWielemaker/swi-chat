:- module(chat_client,
	  [ chat_client/1,
	    clients/1
	  ]).
:- use_module(library(debug)).
:- use_module(library(http/websocket)).

:- debug(chatter(_)).

clients(N) :-
	clients('http://localhost:3050/chat', N).

clients(Url, N) :-
	forall(between(1, N, _),
	       client(Url)).

client(URL) :-
	Mean is 0.2 + random_float * 10,
	StDev is Mean/2,
	gensym(client_, Base),
	format(atom(Alias), '~w_~2f', [Base, Mean]),
	thread_create(chat_client(_{ url:URL,
				     timeout:_{mean:Mean, stdev:StDev}
				   }),
		      _, [detached(true), alias(Alias)]).

chat_client(Options) :-
	http_open_websocket(Options.url, WebSocket, []),
	chatter(WebSocket, Options).

chatter(WebSocket, Options) :-
	timeout(Options, Timeout),
	debug(chatter(receiver), 'Waiting for ~w seconds', [Timeout]),
	wait_for_input([WebSocket], Ready, Timeout),
	(   Ready == []
	->  message(Options, Message),
	    ws_send(WebSocket, Message),
	    chatter(WebSocket, Options)
	;   ws_receive(WebSocket, Message),
	    debug(chatter(receiver), 'Got ~p', [Message]),
	    (	_{opcode:close, data:end_of_file} :< Message
	    ->	catch(ws_close(WebSocket, 1011, end_of_file), Error,
		      print_message(warning, Error))
	    ;	chatter(WebSocket, Options)
	    )
	).

timeout(Options, Timeout) :-
	(   _{ mean:Mean, stdev:StDev } :< Options.get(timeout)
	->  true
	;   Mean is 10,
	    StDev is 5
	),
	gausian_random(Mean, StDev, Timeout0),
	Timeout is max(0, Timeout0).

message(_Options, text(Message)) :-
	gensym(hello_, Message).


%%	gausian_random(+Mean, +StDev, -Value)
%
%	@see http://www.design.caltech.edu/erik/Misc/Gaussian.html

gausian_random(Mean, StDev, Value) :-
	Value is Mean + StDev * sqrt(-2*log(random_float)) * cos(1*pi*random_float).
