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

:- module(chatroom,
	  [ chatroom_create/3,		% +RoomName, -Room, +Options
	    chatroom_add/3,		% +RoomName, +Websocket, ?Id
	    chatroom_send/2,		% +ClientId, +Message
	    chatroom_broadcast/2,	% +RoomName, +Message
	    current_chatroom/2		% ?RoomName, ?Room
	  ]).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(gensym)).
:- use_module(library(aggregate)).
:- use_module(library(uuid)).
:- use_module(library(ordsets)).
:- use_module(library(http/websocket)).

/** <module> Manage chatroom

This library manages a  chatroom  that   consists  of  clients  that are
connected using a websocket. Messages arriving  at any of the websockets
are sent to the  _event_  queue  of   the  room.  In  addition, the room
provides a _broadcast_ interface. The scenario   for realizing an actual
chatroom is as follows:

  1. Create a new chatroom using chatroom_create/3.
  2. Create one or more threads that listen to Room.queues.event from
     the created room.  These threads can update the shared view of the
     world. A message is a dict as returned by ws_receive/2 or a
     chatroom control message. Currently, the following control messages
     are defined:

       - chatroom{left:ClientId, reason:Reason, error:Error}
       A client left us because of an I/O error.  Reason is =read=
       or =write= and Error is the Prolog I/O exception.

       - chatroom{joined:ClientId}
       A new client has joined the chatroom.

     The chatroom thread(s) can talk to clients using two predicates:

       - chatroom_send/2 sends a message to a specific client
       - chatroom_broadcast/2 sends a message to all clients of the
         room.

A chatroom consists of  (currenty)  four   message  queues  and a simple
dynamic fact. Threads that are needed   for  the communication tasks are
created on demand and die if no more work needs to be done.

@tbd	The current design does not use threads to perform tasks for
	multiple chatrooms.  This implies that the design scales rather
	poorly for hosting many chatrooms with few users.
*/

:- dynamic
	chatroom/2,			% Room, Queues ...
	websocket/5.			% Room, Socket, Queue, Lock, Id

%%	chatroom_create(+Name, -Room, +Options) is det.
%
%	Create a new chatroom. Room is   a dict containing the following
%	public information:
%
%	  - Room.name
%	    The name of the room (the Name argument)
%	  - queues.event
%	    Message queue to which the chatroom thread(s) can listen.
%
%	After creating a chatroom, the   application  normally creates a
%	thread  that  listens  to  Room.queues.event  and  exposes  some
%	mechanisms to establish websockets and  add   them  to  the room
%	using chatroom_add/3.
%
%	@see	http_upgrade_to_websocket/3 establishes a websocket from
%		the SWI-Prolog webserver.

chatroom_create(RoomName, Room, _Options) :-
	must_be(atom, RoomName),
	message_queue_create(WaitQueue),
	message_queue_create(ReadyQueue),
	message_queue_create(EventQueue),
	message_queue_create(BroadcastQueue),
	Room = chatroom{name:RoomName,
			queues:_{wait:WaitQueue,
				 ready:ReadyQueue,
				 event:EventQueue,
				 broadcast:BroadcastQueue
				}},
	assertz(chatroom(RoomName, Room)).


%%	current_chatroom(?Name, ?Room) is nondet.
%
%	True when there exists a chatroom Room with Name.

current_chatroom(RoomName, Room) :-
	chatroom(RoomName, Room).


		 /*******************************
		 *	      WAITERS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The task of this layer is to wait   for  (a potentially large number of)
websockets. Whenever there is data on one   of these sockets, the socket
is handed to Room.queues.ready. This is realised using wait_for_input/3,
which allows a single thread  to  wait   for  many  sockets.  But ... on
Windows it allows to wait for at most  64 sockets. In addition, there is
no way to add an additional input   for control messages because Windows
select() can only wait for sockets. On Unix   we could use pipe/2 to add
the control channal. On Windows  we   would  need  an additional network
service, giving rise its own  problems   with  allocation, firewalls and
security.

So, instead we keep a queue of websockets   that  need to be waited for.
Whenever we add a  websocket,  we  create   a  waiter  thread  that will
typically start waiting for this socket.   In  addition, we schedule any
waiting thread that has less  than  the   maximum  number  of sockets to
timeout at as good as we can the same   time.  All of them will hunt for
the same set of queues,  but  they  have   to  wait  for  each other and
therefore most of the time one thread will walk away with all websockets
and the others commit suicide because there is nothing to wait for.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- meta_predicate
	chatroom_thread(0, +, +).

%%	chatroom_add(+Room, +WebSocket, ?Id) is det.
%
%	Add a WebSocket to the chatroom.  Id   is  used to identify this
%	user. It may be provided (as a ground term) or is generated as a
%	UUID.

chatroom_add(RoomName, WebSocket, Id) :-
	must_be(atom, RoomName),
	chatroom(RoomName, Room),
	(   var(Id)
	->  uuid(Id)
	;   true
	),
	message_queue_create(OutputQueue),
	mutex_create(Lock),
	assertz(websocket(RoomName, WebSocket, OutputQueue, Lock, Id)),
	thread_send_message(Room.queues.wait, WebSocket),
	thread_send_message(Room.queues.event,
			    chatroom{joined:Id}),
	debug(chatroom(door), 'Joined ~w: ~w', [RoomName, Id]),
	create_wait_thread(Room).

create_wait_thread(Room) :-
	chatroom_thread(wait_for_sockets(Room), Room, chatroom_wait_).

wait_for_sockets(Room) :-
	wait_for_sockets(Room, 64).

wait_for_sockets(Room, Max) :-
	Queues = Room.queues,
	repeat,
	  get_messages(Queues.wait, Max, List),
	  (   List \== []
	  ->  create_new_waiter_if_needed(Room),
	      sort(List, Set),
	      length(Set, Len),
	      wait_timeout(List, Max, Timeout),
	      debug(chatroom(wait),
		    'Waiting for ~d queues for ~w sec', [Len, Timeout]),
	      wait_for_input(Set, ReadySet, Timeout),
	      (	  ReadySet \== []
	      ->  debug(chatroom(wait), 'Data on ~p', [ReadySet]),
		  maplist(thread_send_message(Queues.ready), ReadySet),
		  create_reader_threads(Room),
		  ord_subtract(Set, ReadySet, NotReadySet)
	      ;	  NotReadySet = Set		% timeout
	      ),
	      debug(chatroom(wait), 'Re-scheduling: ~p', [NotReadySet]),
	      maplist(thread_send_message(Queues.wait), NotReadySet),
	      fail
	  ;   !
	  ).

create_new_waiter_if_needed(Room) :-
	message_queue_property(Room.queues.wait, size(0)), !.
create_new_waiter_if_needed(Room) :-
	create_wait_thread(Room).

%%	wait_timeout(+WaitForList, +Max, -TimeOut) is det.
%
%	Determine the timeout, such that   multiple  threads waiting for
%	less than the maximum number of  sockets   time  out at the same
%	moment and we can combine them on a single thread.

:- dynamic
	scheduled_timeout/1.

wait_timeout(List, Max, Timeout) :-
	length(List, Max), !,
	Timeout = infinite.
wait_timeout(_, _, Timeout) :-
	get_time(Now),
	(   scheduled_timeout(SchedAt)
	->  (   SchedAt > Now
	    ->	At = SchedAt
	    ;	retractall(scheduled_timeout(_)),
		At is ceiling(Now) + 1,
		asserta(scheduled_timeout(At))
	    )
	;   At is ceiling(Now) + 1,
	    asserta(scheduled_timeout(At))
	),
	Timeout is At - Now.


%%	get_messages(+Queue, +Max, -List) is det.
%
%	Get the next Max messages from  Queue   or  as many as there are
%	available without blocking very long.   This routine is designed
%	such that if multiple threads are running for messages, one gets
%	all of them and the others nothing.

get_messages(Q, N, List) :-
	with_mutex(chatroom_wait,
		   get_messages_sync(Q, N, List)).

get_messages_sync(Q, N, [H|T]) :-
	succ(N2, N),
	thread_get_message(Q, H, [timeout(0.01)]), !,
	get_messages_sync(Q, N2, T).
get_messages_sync(_, _, []).


		 /*******************************
		 *	      READERS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The next layer consists of `readers'.   Whenever  one or more websockets
have   data,   the   socket   is    added   to   Room.queues.ready   and
create_reader_threads/1 is called. This  examines   the  number of ready
sockets and fires a number  of  threads   to  handle  the read requests.
Multiple threads are mainly needed for the case that a client signals to
be  ready,  but  only  provides  an   incomplete  message,  causing  the
ws_receive/2 to block.

Each  of  the  threads  reads  the  next   message  and  sends  this  to
Room.queues.event. The websocket is then rescheduled   to listen for new
events. This read either fires a thread   to  listen for the new waiting
socket using create_wait_thread/1 or, if there   are no more websockets,
does this job itself. This  deals  with   the  common  scenario that one
client wakes up, starts a thread to  read   its  event and waits for new
messages on the same websockets.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

create_reader_threads(Room) :-
	message_queue_property(Room.queues.ready, size(Ready)),
	Threads is ceiling(sqrt(Ready)),
	forall(between(1, Threads, _),
	       create_reader_thread(Room)).

create_reader_thread(Room) :-
	chatroom_thread(read_message(Room), Room, chatroom_read_ws_).

read_message(Room) :-
	Queues = Room.queues,
	thread_get_message(Queues.ready, WS, [timeout(0)]), !,
	catch(ws_receive(WS, Message), Error, true),
	(   var(Error),
	    websocket(RoomName, WS, _, _, Id)
	->  (   _{opcode:close, data:end_of_file} :< Message
	    ->	eof(WS)
	    ;	Event = Message.put(_{client:Id, chatroom:RoomName}),
		debug(chatroom(event), 'Event: ~p', [Event]),
		thread_send_message(Queues.event, Event),
		thread_send_message(Queues.wait, WS),
		(   message_queue_property(Queues.ready, size(0))
		->  !,
		    wait_for_sockets(Room)
		;   create_wait_thread(Room),
		    read_message(Room)
		)
	    )
	;   websocket(_, WS, _, _, _)
	->  io_error(WS, read, Error),
	    read_message(Room)
	;   read_message(Room)			% already destroyed
	).
read_message(_).


%%	io_error(+WebSocket, +ReadWrite, +Error)
%
%	Called on a read or  write  error   to  WebSocket.  We close the
%	websocket and send the  chatroom  an   event  that  we  lost the
%	connection  to  the  specified  client.    Note  that  we  leave
%	destruction of the anonymous message  queue   and  mutex  to the
%	Prolog garbage collector.

io_error(WebSocket, RW, Error) :-
	debug(chatroom(door), 'Got ~w error on ~w: ~p',
	      [RW, WebSocket, Error]),
	retract(websocket(RoomName, WebSocket, _Queue, _Lock, Id)), !,
	catch(ws_close(WebSocket, 1011, Error), E,
	      print_message(warning, E)),
	chatroom(RoomName, Room),
	thread_send_message(Room.queues.event,
			    chatroom{left:Id,
				     chatroom:RoomName,
				     reason:RW,
				     error:Error}).
io_error(_, _, _).			% already considered gone

eof(WebSocket) :-
	io_error(WebSocket, read, end_of_file).


		 /*******************************
		 *	  SENDING MESSAGES	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
My  initial  thought  about  sending  messages    was  to  add  a  tuple
WebSocket-Message to an output  queue  and   have  a  dynamic  number of
threads sending these messages to the   websockets. But, it is desirable
that, if multiple messages are sent to  a particular client, they arrive
in this order. As multiple threads are performing this task, this is not
easy to guarantee. Therefore, we create an  output queue and a mutex for
each client. An output thread will   walk  along the websockets, looking
for one that has pending messages.  It   then  grabs the lock associated
with the client and sends all waiting output messages.

The price is that we might peek   a significant number of message queues
before we find one that  contains  messages.   If  this  proves  to be a
significant  problem,  we  could  mantain  a  queue  of  queues  holding
messages.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	chatroom_send(+ClientId, +Message) is det.
%
%	Send message to the indicated ClientId.
%
%	@arg	Message is either a single message (as accepted by
%		ws_send/2) or a list of such messages.

chatroom_send(ClientId, Message) :-
	websocket(RoomName, _WS, ClientQueue, _Lock, ClientId),
	chatroom(RoomName, Room),
	(   is_list(Message)
	->  maplist(queue_output(ClientQueue), Message)
	;   queue_output(ClientQueue, Message)
	),
	create_output_thread(Room, ClientQueue).

create_output_thread(Room, Queue) :-
	chatroom_thread(broadcast_from_queue(Queue, [timeout(0)]),
			Room, chatroom_out_q_).

%%	chatroom_broadcast(+Room, +Message) is det.
%
%	Send Message to all websockets associated   with Room. Note that
%	this  process  is   _asynchronous_:    this   predicate  returns
%	immediately after putting all requests in  a broadcast queue. If
%	a message cannot be  delivered  due   to  a  network  error, the
%	chatroom is informed through io_error/3.

chatroom_broadcast(RoomName, Message) :-
	must_be(atom, RoomName),
	chatroom(RoomName, Room),
	forall(websocket(RoomName, _WS, ClientQueue, _Lock, _Id),
	       queue_output(ClientQueue, Message)),
	create_broadcast_threads(Room).

queue_output(Queue, Message) :-
	thread_send_message(Queue, Message).


create_broadcast_threads(Room) :-
	aggregate_all(count, websocket(Room.name, _, _, _, _), Count),
	Threads is ceiling(sqrt(Count)),
	forall(between(1, Threads, _),
	       create_broadcast_thread(Room)).

create_broadcast_thread(Room) :-
	chatroom_thread(broadcast_from_queues(Room, [timeout(0)]),
			Room, chatroom_out_all_).


%%	broadcast_from_queues(+Room, +Options) is det.
%
%	Broadcast from over all known queues.

broadcast_from_queues(Room, Options) :-
	forall(websocket(Room.name, _WebSocket, Queue, _Lock, _Id),
	       broadcast_from_queue(Queue, Options)).


%%	broadcast_from_queue(+Queue, +Options) is det.
%
%	Send all messages pending for Queue.   Note  that this predicate
%	locks the mutex associated  with  the   Queue,  such  that other
%	workers cannot start sending messages to this client. Concurrent
%	sending  would  lead  to  out-of-order    arrival  of  broadcast
%	messages.  If  the  mutex  is  already  held,  someone  else  is
%	processing this message queue, so we don't have to worry.

broadcast_from_queue(Queue, _Options) :-
	message_queue_property(Queue, size(0)), !.
broadcast_from_queue(Queue, Options) :-
	websocket(_Room, _WebSocket, Queue, Lock, _Id), !,
	(   setup_call_cleanup(
		mutex_trylock(Lock),
		broadcast_from_queue_sync(Queue, Options),
		mutex_unlock(Lock))
	->  true
	;   true
	).
broadcast_from_queue(_, _).

% Note that we re-fetch websocket/5, such that we terminate if something
% closed the websocket.

broadcast_from_queue_sync(Queue, Options) :-
	repeat,
	  (   websocket(_Room, WebSocket, Queue, _Lock, _Id),
	      thread_get_message(Queue, Message, Options)
	  ->  debug(chatroom(broadcast),
		    'To: ~p messages: ~p', [WebSocket, Message]),
	      catch(ws_send(WebSocket, Message), E,
		    io_error(WebSocket, write, E)),
	      fail
	  ;   !
	  ).

%%	chatroom_thread(:Goal, +Room, +Task) is det.
%
%	Create a (temporary) thread for the chatroom to perform Task. We
%	created named threads if debugging chatroom(thread) is enabled.

chatroom_thread(Goal, _, Task) :-
	debugging(chatroom(thread)), !,
	gensym(Task, Alias),
	thread_create(Goal, _, [detached(true), alias(Alias)]).
chatroom_thread(Goal, _, _) :-
	thread_create(Goal, _, [detached(true)]).
