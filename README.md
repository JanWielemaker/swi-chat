# SWI-Prolog based chat server

This repository provides a demonstration for   using  the recently added
websocket support for realising a  chat  server.   To  use  it, you must
install *[SWI-Prolog](http://www.swi-prolog.org) 7.1.19 or later*. Then,
you can load `chat.pl` and run

    ?- server.

This will start the server at port 3050.

## Status

The chatserver itself is really simple.   Tested with the server running
on Linux using firefox and chromium as clients.

## Future

The library chatroom.pl is probably going to   end  up as a core library
and the demo program will than be added as a demo application.
