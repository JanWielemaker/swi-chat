/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(chat_server,
	  [ server/0,
	    server/1
	  ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/websocket)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(debug)).

:- use_module(chatroom).

% This is a demo.  Be chatty.  Comment for silent operation
:- debug(websocket).
:- debug(chat).

/** <module> A scalable websocket based chat server in SWI-Prolog

Chat servers are an example of   services  that require mixed initiative
and may be used to serve many connections. One way to implement is using
long-polling: the browser asks for events   from  the server. The server
waits until there is an event or  it   times  out  after -say- 1 minute,
after which the server replies there are  no events and the client tries
again. The long polling structure can   be implemented in the SWI-Prolog
server architecture, but it is  rather   expensive  because it implies a
Prolog thread for each blocking call.

This demo application implements  a   chatroom  using  _websockets_. The
implementation uses chatroom.pl, which bundles   the  responsibility for
multiple  websockets  in  a  small  number   of  threads  by  using  I/O
multiplexing based on wait_for_input/3. As a   user of chatroom.pl, life
is fairly straighforward:

  - Chreate a chatroom using chatroom_create/3 and a thread that
    listens to chat events and broadcasts the changes.

  - Serve a web page that provides the chat frontend.  The frontend
    contains JavaScript that establishes a websocket on /chat.  If
    a websocket is obtained, hand it to to the room using
    chatroom_add/2
*/


%%	server is det.
%%	server(?Port) is det.
%
%	Create the chat room and start the   server. The default port is
%	3050.

server :-
	server(3050).

server(Port) :-
	(   debugging(chat)
	->  prolog_ide(thread_monitor)
	;   true
	),
	create_chat_room,
	http_server(http_dispatch, [port(Port)]),
	format(user_error, 'Started server at http://localhost:~d/~n', [Port]).

% setup the HTTP location. The  first   (/)  loads  the application. The
% loaded application will create  a   websocket  using  /chat. Normally,
% http_upgrade_to_websocket/3 runs call(Goal, WebSocket)  and closes the
% connection if Goal terminates. Here, we use guarded(false) to tell the
% server we will take responsibility for the websocket.

:- http_handler(root(.),    chat_page,      []).
:- http_handler(root(chat),
		http_upgrade_to_websocket(
		    accept_chat,
		    [ guarded(false),
		      subprotocols([chat])
		    ]),
		[ id(chat_websocket)
		]).

chat_page(_Request) :-
	reply_html_page(
	    title('SWI-Prolog chat demo'),
	    \chat_page).

%%	chat_page//
%
%	Generate the web page. To  keep   everything  compact  we do the
%	styling inline.

chat_page -->
	html([ h1('YAWSBCR: Yet Another ...'),
	       div([ id(chat),
		     style('height: 150px; overflow-y:scroll;'+
			   'border: solid 1px black; padding:5px')
		   ], []),
	       input([ placeholder('Type a message and hit RETURN'),
		       id(input),
		       onkeypress('handleInput(event)'),
		       style('width:100%; border:solid 1px black;'+
			     'padding: 5px; box-sizing: border-box')
		     ], [])
	     ]),
	script.

%%	script//
%
%	Generate the JavaScript  that  establishes   the  websocket  and
%	handles events on the websocket.

script -->
	{ http_link_to_id(chat_websocket, [], WebSocketURL)
	},
	js_script({|javascript(WebSocketURL)||
function handleInput(e) {
  if ( !e ) e = window.event;  // IE
  if ( e.keyCode == 13 ) {
    var msg = document.getElementById("input").value;
    sendChat(msg);
    document.getElementById("input").value = "";
  }
}

var connection;

function openWebSocket() {
  connection = new WebSocket("ws://"+window.location.host+WebSocketURL,
			     ['chat']);

  connection.onerror = function (error) {
    console.log('WebSocket Error ' + error);
  };

  connection.onmessage = function (e) {
    var chat = document.getElementById("chat");
    var msg = document.createElement("div");
    msg.appendChild(document.createTextNode(e.data));
    var child = chat.appendChild(msg);
    child.scrollIntoView(false);
  };
}

function sendChat(msg) {
  connection.send(msg);
}

window.addEventListener("DOMContentLoaded", openWebSocket, false);
		  |}).


%%	accept_chat(+WebSocket) is det.
%
%	Normally,  the  goal  called    by   http_upgrade_to_websocket/3
%	processes all communication with the   websocket in a read/write
%	loop. In this case however,  we tell http_upgrade_to_websocket/3
%	that we will take responsibility for   the websocket and we hand
%	it to the chat room.

accept_chat(WebSocket) :-
	chatroom_add(chat, WebSocket, _Id).

%%	create_chat_room
%
%	Create our actual chat room.

:- dynamic
	utterance/1,			% messages
	visitor/1.			% joined visitors

create_chat_room :-
	chatroom_create(chat, Room, _{}),
	thread_create(chatroom(Room), _, [alias(chatroom)]).

%%	chatroom(+Room)
%
%	Realise the chatroom main loop: listen  for an event, update the
%	state and possibly broadcast status updates.

chatroom(Room) :-
	thread_get_message(Room.queues.event, Message),
	handle_message(Message, Room),
	chatroom(Room).

handle_message(Message, Room) :-
	websocket{opcode:text} :< Message, !,
	assertz(utterance(Message)),
	chatroom_broadcast(Room.name, Message).
handle_message(Message, _Room) :-
	chatroom{joined:Id} :< Message, !,
	assertz(visitor(Id)),
	forall(utterance(Utterance),
	       chatroom_send(Id, Utterance)).
handle_message(Message, _Room) :-
	chatroom{left:Id} :< Message, !,
	retractall(visitor(Id)).
handle_message(Message, _Room) :-
	debug(chat, 'Ignoring message ~p', [Message]).
