:- module(
  lle_settings,
  [
    lle_graph/2 % +Mode:oneof([collection,dissemination]),
                        % -DefaultGraph:iri
  ]
).

/** <module> LOD Laudromat endpoint: settings

Settings for operating the LOD Laundromat endpoint.

@author Wouter Beek
@version 2014/08
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- rdf_register_prefix(ll, 'http://lodlaundromat.org/vocab#').



%! lle_graph(+Mode:oneof([collection,dissemination]), -Graph:iri) is det.

lle_graph(Mode, DefaultGraph):-
  lle_version(Mode, Version),
  atom_number(Fragment, Version),
  lle_uri_components(uri_components(Scheme,Authority,_,_,_)),
  uri_components(
    DefaultGraph,
    uri_components(Scheme,Authority,_,_,Fragment)
  ).

lle_uri_components(uri_components(http,'lodlaundromat.org','','','')).

lle_version(collection, 11).
lle_version(dissemination, 10).

