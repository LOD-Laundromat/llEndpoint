:- module(conf_lle, []).

:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).

:- use_module(cliopatria(hooks)).
   cliopatria:menu_item(500=places/lwm, 'LOD Washing Machine').
   cliopatria:menu_item(600=places/plTabular, plTabular).

:- if(\+ current_module(load_project)).
  :- ensure_loaded('../load').
:- endif.


% Serves clean data documents.
:- http_handler(
  cliopatria(data),
  serve_files_in_directory_with_cors(data),
  [id(data),prefix]
).


% Load the LOD Laundromat schema.
:- use_module(lle_schema(schema)).


% LOD Basket.
:- use_module(lle_basket(basket_web)).
:- http_handler(cliopatria(basket), basket, []).


% plTabular endpoint.
:- use_module(plTabular(rdf_tabular)).
:- http_handler(cliopatria(plTabular), rdf_tabular, [id(plTabular)]).

rdf_tabular(Request):-
  rdf_tabular(Request, plTabular).

:- ensure_loaded(plServer(style)).

:- multifile(user:body//2).
user:body(plTabular, Body) -->
  html_requires(css('plServer.css')),
  user:body(cliopatria(default), Body).


% llInfobox endpoint.
:- use_module(lle(ll_infobox)).
:- http_handler(cliopatria(infobox), ll_infobox, [id(llInfobox)]).


% LOD Washing Machine endpoint.
:- use_module(lle_deb(lwm_web_deb)).
:- http_handler(cliopatria(lwm), lwm_web_deb, [id(lwm)]).

lwm_web_deb(Request):-
  lwm_web_deb(Request, cliopatria(default)).

