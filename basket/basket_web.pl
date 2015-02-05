:- module(basket_web, []).

/** <module> LOD Basket: Endpoint

Web-based front-end to the LOD basket.

@author Wouter Beek
@version 2014/06, 2014/08, 2015/01-2015/02
*/

:- use_module(library(http/http_cors)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(generics(typecheck)).

:- use_module(lle(basket/basket)).

:- http_handler(root(basket), basket, [id(llBasket)]).





basket(Request):-
  cors_enable,
  (   catch(
        http_parameters(Request, [uri(Uri,[atom])]),
        _,
        fail
      ),
      is_uri(Uri)
  ->  % Make sure that it is a URL.
      add_to_basket(Uri),
      % HTTP status code 202 Accepted: The request has been accepted
      % for processing, but the processing has not been completed.
      reply_json(json{}, [status(202)])
  ;   % HTTP status code 400 Bad Request: The request could not
      % be understood by the server due to malformed syntax.
      reply_json(json{}, [status(400)])
  ).

