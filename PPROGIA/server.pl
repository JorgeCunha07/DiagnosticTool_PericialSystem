% 1. consult("server.pl").
% 2. server(3000).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

:- http_handler(root(.), start, []).

server(Port) :-
    consult("motorInferencia.pl"),
    http_server(http_dispatch, [port(Port)]).

start(_Request) :-		
    format('Content-type: text/plain~n~n'),
    format('Até estala!~n').
