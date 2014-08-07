:- module(conf_lle, []).

:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).

:- use_module(cliopatria(hooks)).
   cliopatria:menu_item(500=places/lwm, 'LOD Washing Machine').
   cliopatria:menu_item(600=places/plTabular, plTabular).

:- if(\+ current_module(load_project)).
  :- ensure_loaded('../debug').
  :- ensure_loaded('../load').
:- endif.

:- http_handler(cliopatria(basket), basket, []).

:- ensure_loaded(plServer(style)).



% Load the LOD Laundromat schema.

:- use_module(lle_schema(ll_schema)).



% plTabular

:- http_handler(cliopatria(plTabular), rdf_tabular, [id(plTabular)]).

:- use_module(plTabular(rdf_tabular)).
rdf_tabular(Request):-
  rdf_tabular(Request, plTabular).

:- multifile(user:body//2).
user:body(plTabular, Body) -->
  html_requires(css('plServer.css')),
  user:body(cliopatria(default), Body).



% LOD InfoBox

:- http_handler(cliopatria(infobox), ll_infobox, [prefix]).

:- use_module(lle(ll_infobox)).



% LOD-Washing-Machine

:- http_handler(cliopatria(lwm), lwm_web, [prefix]).

:- use_module(lle(lwm_web)).
lwm_web(Request):-
  lwm_web(Request, cliopatria(default)).

