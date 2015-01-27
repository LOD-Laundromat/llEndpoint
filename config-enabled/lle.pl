:- module(conf_lle, []).

% Load project.
:- if(\+ current_module(load_project)).
  :- if(current_prolog_flag(argv, ['--debug'])).
    :- ensure_loaded('../debug').
  :- else.
    :- ensure_loaded('../load').
  :- endif.
:- endif.



:- use_module(cliopatria(hooks)).

:- use_module(plTabular(rdf_tabular)).

:- use_module(lle(lle_data)).
:- use_module(lle(basket/basket_web)).
:- use_module(lle(debug/lwm_deb)).
:- use_module(lle(debug/lwm_deb_errors)).
:- use_module(lle(debug/lwm_deb_progress)).

user:current_html_style(menu_page).


% ClioPatria menu items.
:- multifile(cliopatria:menu_item/2).
cliopatria:menu_item(500=lle/lwm_deb_errors, 'LOD Errors').
cliopatria:menu_item(500=lle/lwm_deb_progress, 'LOD Progress').
cliopatria:menu_item(600=lle/plTabular, plTabular).



% Web root.
:- dynamic(http:location/3).
:- multifile(http:location/3).
http:location(lle, cliopatria(lle), []).

