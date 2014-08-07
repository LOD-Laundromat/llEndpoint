% Run file for the llEndpoint project.

:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).

:- if(current_prolog_flag(argv, ['--debug'])).
  :- ensure_loaded(debug).
:- else.
  :- ensure_loaded(load).
:- endif.



% Starts a Prolog server.

:- use_module(load_project).
:- load_subproject(dh, plServer).

:- use_module(plServer(app_server)).
:- use_module(plServer(plServer)).
:- use_module(plServer(web_modules)). % Web module registration.

:- start_app_server([]).



% Serves clean data documents.

:- http_handler(
  root(data),
  serve_files_in_directory_with_cors(data),
  [id(data),prefix]
).



% Load the LOD Laundromat schema.

:- use_module(lle_schema(schema)).



% LOD Basket endpoint.

:- use_module(lle_basket(basket_web)).
:- http_handler(root(basket), basket, [id(llBasket)]).



% plTabular endpoint.

:- use_module(plTabular(rdf_tabular)).
:- http_handler(root(tab), rdf_tabular, [id(plTabular)]).

:- dynamic(user:web_module/2).
:- multifile(user:web_module/2).
   user:web_module('plTabular', rdf_tabular).

rdf_tabular(Request):-
  rdf_tabular(Request, plServer_style).

:- html_resource(plTabular, [requires([css('plTabular.css')]),virtual(true)]).



% llInfobox endpoint.

:- http_handler(root(infobox), ll_infobox, [id(llInfobox)]).



% LOD Washing Machine endpoint.

:- use_module(lle_deb(lwm_web_deb)).
:- http_handler(root(lwm), lwm_web_deb, [id(lwm)]).

:- dynamic(user:web_module/2).
:- multifile(user:web_module/2).
   user:web_module('LOD Washing Machine', lwm_web_deb).

lwm_web_deb(Request):-
  lwm_web_deb(Request, plServer_style).

