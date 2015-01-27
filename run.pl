% Standalone startup file for the LOD Laundromat endpoint.

% Load modules.
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



% Default HTML style.
user:current_html_style(menu_page).



% Web root.
:- multifile(http:location/3).
:- dynamic(http:location/3).
http:location(lle, /, []).



% Register Web modules.
:- dynamic(user:web_module/2).
:- multifile(user:web_module/2).
user:web_module(plTabular, rdf_tabular).
user:web_module('LOD Errors', ll_web_errors).
user:web_module('LOD Progress', ll_web_progress).

