:- module(
  basket,
  [
    add_to_basket/1 % +Uri:atom
  ]
).

/** <module> LOD Basket

@author Wouter Beek
@version 2014/06, 2014/08, 2015/01-2015/02
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plXsd(dateTime/xsd_dateTime_functions)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).





%! add_to_basket(+Uri:atom) is det.

add_to_basket(Uri):-
/* @tbd Is this needed?
  % If the given argument is an IRI, then non-URL Unicode characters
  % may appear in it unescaped. This conversion escapes such characters
  % to ensure a valid URL.
  uri_iri(Uri, Uri),
*/
  with_mutex(lle_basket, (
    rdf_atom_md5(Uri, 1, Md5),
    (   % The URL has already been added.
        rdf_typed_literal(Resource, llo:md5, Md5, xsd:string),
        rdf_has(Resource, llo:added, _)
    ->  print_message(informational, already_added(Md5))
    ;   add_to_basket(Md5, Uri)
    )
  )).


%! add_to_basket(+Md5:atom, +Uri:atom) is det.

add_to_basket(Md5, Uri):-
  rdf_global_id(ll:Md5, Datadoc),
  rdf_assert_instance(Datadoc, llo:'URL'),
  rdf_assert_typed_literal(Datadoc, llo:md5, Md5, xsd:string),
  rdf_assert(Datadoc, llo:url, Uri),
  get_dateTime(Added),
  rdf_assert_typed_literal(Datadoc, llo:added, Added, xsd:dateTime).





% MESSAGES %

:- multifile(prolog:message//1).

prolog:message(already_added(Md5)) -->
  cannot_add(Md5),
  ['already added'].

cannot_add(Md5) -->
  ['MD5 ~w cannot be added to the pool: '-[Md5]].

