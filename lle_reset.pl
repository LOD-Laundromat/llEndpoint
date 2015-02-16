:- module(lle_reset, []).

/** <module> LOD Laundromat Endpoint: Reset

Web Service for resetting LOD Laundromat documents.

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(user(user_db)).

:- use_module(generics(typecheck)).

:- http_handler('/reset', reset, [id(llReset)]).





reset(Request):-
  cors_enable,
  (   http_parameters(Request, [datadoc(Datadoc,[atom])]),
      is_uri(Datadoc)
  ->  remove_datadoc(Datadoc),
      format('~d', [200])
  ;   format('~d', [400])
  ).


%! remove_datadoc(+Datadoc:atom) is det.
% Removes the scraping metadata for the given data document
% from the store.

remove_datadoc(Datadoc):-
  authorized(write([], sparql)),
  aggregate_all(
    set(Warning),
    rdf_has(Datadoc, llo:warning, Warning),
    Warnings
  ),
  maplist(remove_warning, Warnings),
  rdf_retractall(Datadoc, _, _).


%! remove_warning(+Warning:atom) is det.
% Removes a LOD Washing Machine warning message from the store.

remove_warning(Warning):-
  aggregate_all(
    set(StreamPosition),
    rdf_has(Warning, error:streamPosition, StreamPosition),
    StreamPositions
  ),
  maplist(remove_stream_position, StreamPositions),
  rdf_retractall(Warning, _, _).


%! remove_stream_position(+StreamPosition:atom) is det.
% Removes a LOD Washing Machine stream position from the store.

remove_stream_position(StreamPosition):-
  rdf_retractall(StreamPosition, _, _).

