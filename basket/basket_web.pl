:- module(basket_web, []).

/** <module> LOD Basket: Endpoint

Web-based front-end to the LOD basket.

@author Wouter Beek
@version 2014/06, 2014/08, 2015/01
*/

:- use_module(library(http/http_cors)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- use_module(generics(typecheck)).

:- use_module(plXsd(dateTime/xsd_dateTime_functions)).

:- use_module(lle(lle_settings)).

:- http_handler(root(basket), basket, [id(llBasket)]).





basket(Request):-
  cors_enable,
  (   catch(
        http_parameters(
          Request,
          [url(Url,[atom]),version(Version,[nonneg])]
        ),
        _,
        fail
      ),
      is_uri(Url)
  ->  % Make sure that it is a URL.
      add_to_basket(Version, Url),
      
      % HTTP status code 202 Accepted: The request has been accepted
      % for processing, but the processing has not been completed.
      reply_json(json{}, [status(202)])
  ;   % HTTP status code 400 Bad Request: The request could not
      % be understood by the server due to malformed syntax.
      reply_json(json{}, [status(400)])
  ).


%! add_to_basket(+Version:positive_integer, +Url:url) is det.

add_to_basket(Version, Url1):-
  % If the given argument is an IRI, then non-URL Unicode characters
  % may appear in it unescaped. This conversion escapes such characters
  % to ensure a valid URL.
  uri_iri(Url2, Url1),
  with_mutex(lle_basket, (
    rdf_atom_md5(Url2, 1, Md5),
    (   % The URL has already been added.
        rdf(Resource, llo:md5, Md5),
        rdf(Resource, llo:added, _)
    ->  print_message(informational, already_added(Md5))
    ;   store_url(Version, Md5, Url2)
    )
  )).


%! store_url(+Version:positive_integer, +Md5:atom, +Url:url) is det.

store_url(Version, Md5, Url):-
  lle_version_graph(Version, Graph),
  rdf_global_id(ll:Md5, Datadoc),
  rdf_assert(Datadoc, rdf:type, llo:'URL', Graph),
  rdf_assert(Datadoc, llo:md5, literal(type(xsd:string,Md5)), Graph),
  rdf_assert(Datadoc, llo:url, Url, Graph),
  get_dateTime(Added),
  rdf_assert(Datadoc, llo:added, literal(type(xsd:dateTime,Added)), Graph).





% MESSAGES %

:- multifile(prolog:message//1).

prolog:message(already_added(Md5)) -->
  cannot_add(Md5),
  ['already added'].

cannot_add(Md5) -->
  ['MD5 ~w cannot be added to the pool: '-[Md5]].

