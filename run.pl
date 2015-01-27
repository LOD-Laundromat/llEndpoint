% Standalone startup file for the LOD Laundromat endpoint.

:- if(current_prolog_flag(argv, ['--debug'])).
  :- ensure_loaded(debug).
:- else.
  :- ensure_loaded(load).
:- endif.



:- use_module(load_project).
:- load_subproject(lle, plServer).

:- use_module(plServer(app_server)).
:- use_module(plServer(web_modules)). % Web module registration.
:- use_module(plServer(templates/menu_page)). % HTML Template.

:- start_app_server.


% Serve clean data documents.
:- use_module(library(http/http_server_files)).
:- http_handler(
  root(data),
  serve_files_in_directory_with_cors(data),
  [id(data),prefix]
).


:- dynamic(user:web_module/2).
:- multifile(user:web_module/2).

:- multifile(http:location/3).
:- dynamic(http:location/3).

http:location(lle, /, []).

user:current_html_style(menu_page).

% LOD Basket.
:- use_module(lle(basket/basket_web)).

% plTabular.
:- use_module(plTabular(rdf_tabular)).
user:web_module(plTabular, rdf_tabular).

% LOD Washing Machine endpoint.
:- use_module(lle(debug/lwm_deb)).
user:web_module('LOD Washing Machine', lwm_deb).

