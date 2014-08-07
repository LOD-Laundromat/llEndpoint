:- module(
  basket_web,
  [
    basket/1 % +Request:list(nvpair)
  ]
).

/** <module> LOD Laundromat: basket web

Web-based front-end to the LOD basket.

@author Wouter Beek
@version 2014/06, 2014/08
*/

:- use_module(library(http/http_client)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- use_module(generics(typecheck)).
:- use_module(xsd(xsd_dateTime_ext)).

:- use_module(lle(lle_settings)).



basket(Request):-
  cors_enable,
  (
    catch(http_parameters(Request, [url(Url, [])]), _, fail),
    is_url(Url)
  ->
    % Make sure that it is a URL.
    add_to_basket(Url),

    % HTTP status code 202 Accepted: The request has been accepted
    % for processing, but the processing has not been completed.
    reply_json(json{}, [status(202)])
  ;
    % HTTP status code 400 Bad Request: The request could not
    % be understood by the server due to malformed syntax.
    reply_json(json{}, [status(400)])
  ).


%! add_to_basket(+Source) is det.

add_to_basket(Url1):-
  uri_iri(Url2, Url1),
  with_mutex(ll_basket, (
    rdf_atom_md5(Url2, 1, Md5),
    (
      % The URL has already been added.
      rdf(Resource, ll:md5, Md5),
      rdf(Resource, ll:added, _)
    ->
      print_message(informational, already_added(Md5))
    ;
      store_url(Md5, Url2)
    )
  )).


%! store_url(+Md5:atom, +Url:url) is det.

store_url(Md5, Url):-
  lle_graph(dissemination, Graph),
  rdf_global_id(ll:Md5, Resource),
  rdf_assert(Resource, rdf:type, ll:'URL', Graph),
  rdf_assert(Resource, ll:md5, literal(type(xsd:string,Md5)), Graph),
  rdf_assert(Resource, ll:url, Url, Graph),
  get_dateTime(Added),
  rdf_assert(Resource, ll:added, literal(type(xsd:dateTime,Added)), Graph).



% Messages

:- multifile(prolog:message//1).

prolog:message(already_added(Md5)) -->
  cannot_add(Md5),
  ['already added'].

cannot_add(Md5) -->
  ['MD5 ~w cannot be added to the pool: '-[Md5]].

