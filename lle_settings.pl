:- module(
  lle_settings,
  [
    init_lle_settings/1 % +Port:nonneg
  ]
).

/** <module> LOD Laundromat Endpoint: Settings

Settings for the LOD Laundromat Endpoint.

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(settings)).
:- use_module(library(uri)).

:- use_module(plSparql(sparql_db)).

:- dynamic(user:prolog_file_type/2).
:- multifile(user:prolog_file_type/2).

user:prolog_file_type(conf, configuration).

:- setting(
  endpoint,
  oneof([both,cliopatria,virtuoso]),
  both,
  'The endpoint that is used to store the crawling metadata in.'
).





init_lle_settings(Port):-
  (   absolute_file_name(
        lle(settings),
        File,
        [access(read),file_errors(fail),file_type(configuration)]
      )
  ->  load_settings(File)
  ;   true
  ),

  % Register the ClioPatria SPARQL endpoint.
  uri_authority_components(Authority, uri_authority(_,_,localhost,Port)),
  uri_components(Uri, uri_components(http,Authority,'/',_,_)),
  sparql_register_endpoint(cliopatria, [Uri], cliopatria),

  % Virtuoso: Query.
  sparql_register_endpoint(
    virtuoso_query,
    ['http://sparql.backend.lodlaundromat.org'],
    virtuoso
  ),
  sparql_db:assert(
    sparql_endpoint_option0(virtuoso_query, path_suffix(query), '')
  ).

