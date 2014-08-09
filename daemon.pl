#!/usr/bin/env swipl

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Script to start the SWI-Prolog chat  server   as  a  Unix daemon process
based on library(http/http_unix_daemon). The  server   is  started  as a
deamon using

  % ./daemon.pl port=Port [option ...]

See library(http/http_unix_daemon) for details. Using ./deamon.pl --help
for a brief help message.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- set_prolog_flag(verbose, silent).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(broadcast)).
:- use_module(chat).

% http_daemon/0 processes the commandline options, creates the requested
% daemon setup and starts the HTTP server.   It is not allowed to create
% any threads before calling http_daemon/0  and   therefore  we  use the
% http_daemon broadcast event `http(pre_server_start)` to start our chat
% thread.

:- initialization http_daemon.

:- listen(http(pre_server_start), create_chat_room).
