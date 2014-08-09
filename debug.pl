:- use_module(library(debug)).

% This is a demo.  Be chatty.  Comment for silent operation
% :- debug(websocket).
% :- debug(chatroom(wait)).		% quite noisy
:- debug(chatroom(event)).		% new events
:- debug(chatroom(door)).		% visitors joining and leaving
:- debug(chatroom(broadcast)).		% messages sent
:- debug(chatroom(thread)).		% give threads a name
:- debug(chat).				% this demo
:- debug_message_context(+time).	% add timestamp to debug message

:- use_module(chat).
