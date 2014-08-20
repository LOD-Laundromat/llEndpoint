:- module(conf_lle, []).

:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).

:- use_module(cliopatria(hooks)).
   cliopatria:menu_item(500=places/lwm, 'LOD Washing Machine').
   cliopatria:menu_item(600=places/plTabular, plTabular).

:- if(\+ current_module(load_project)).
  :- ensure_loaded('../load').
:- endif.


% #1: Serves clean data documents.
:- use_module(lle(lle_data)).
%:- dynamic(http:location/3).
%:- multifile(http:location/3).
%   http:location(lle_data, root(data), []).
:- http_handler(root(data), clean_data, [prefix]).


% Load the LOD Laundromat schema.
:- use_module(lle_schema(schema)).


% #2: LOD Basket.
:- use_module(lle_basket(basket_web)).
:- http_handler(cliopatria(basket), basket, []).


% HTML style.
:- ensure_loaded(plServer(style)).
:- multifile(user:body//2).
user:body(lle, Body) -->
  html_requires(css('plServer.css')),
  user:body(cliopatria(default), Body).



% #3: plTabular endpoint.
:- use_module(plTabular(rdf_tabular)).
:- http_handler(cliopatria(plTabular), rdf_tabular, [id(plTabular)]).

rdf_tabular(Request):-
  rdf_tabular(Request, lle).


% #4: llInfobox endpoint.
:- use_module(lle(ll_infobox)).
:- http_handler(cliopatria(infobox), ll_infobox, [id(llInfobox)]).


% #5: LOD Washing Machine debug endpoint.
:- use_module(lle_deb(lwm_deb)).

:- dynamic(http:location/3).
:- multifile(http:location/3).
   http:location(lwm_deb, cliopatria(deb), []). 

:- http_handler(lwm_deb(.), lwm_deb, [id(lwm),prefix,priority(-1)]).

lwm_deb(Request):-
  lwm_deb(Request, lle).

