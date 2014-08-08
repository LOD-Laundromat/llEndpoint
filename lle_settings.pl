:- module(
  lle_settings,
  [
    lle_graph/1, % -Graph:iri
    lle_graph/2, % +Version:positive_integer
                 % -Graph:iri
    lle_version/1 % -Version:positive_integer
  ]
).

/** <module> LOD Laudromat endpoint: settings

Settings for operating the LOD Laundromat endpoint.

@author Wouter Beek
@version 2014/08
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- use_module(generics(typecheck)).

:- use_module(lle_schema(schema)).

:- rdf_register_prefix(ll, 'http://lodlaundromat.org/vocab#').



%! lle_graph(-Graph:iri) is det.

lle_graph(Graph):-
  lle_version(Version),
  lle_graph(Version, Graph).


%! lle_graph(+Version:positive_integer, -Graph:iri) is det.

lle_graph(Version, Graph):-
  positive_integer(Version),
  atom_number(Fragment, Version),
  lle_uri_components(uri_components(Scheme,Authority,_,_,_)),
  uri_components(Graph, uri_components(Scheme,Authority,_,_,Fragment)),
  init_schema(Graph).


lle_uri_components(uri_components(http,'lodlaundromat.org','','','')).


lle_version(10).

