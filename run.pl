% Standalone startup file for the LOD Laundromat endpoint.

% Load modules.
:- if(current_prolog_flag(argv, ['--debug'])).
  :- ensure_loaded(debug).
:- else.
  :- ensure_loaded(load).
:- endif.



:- use_module(load_project).
:- load_subproject(lle, plServer).

:- use_module(plc(server/app_server)).

:- use_module(plHtml(web_modules)). % Web module registration.
:- use_module(plHtml(templates/menu_page)). % HTML Template.

:- use_module(lle(lle_settings)).

:- initialization(init).



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





% INITIALIZATION %

init:-
  absolute_file_name(data(.), DefaultDir, [file_type(directory)]),
  OptSpec= [
    [
      default(false),
      help('Whether debug messages are displayed or not.'),
      longflags([debug]),
      opt(debug),
      type(boolean)
    ],
    [
      default(DefaultDir),
      help('The directory where the cleaned data is stored.'),
      longflags([dir,directory]),
      opt(directory),
      type(atom)
    ],
    [
      default(false),
      help('Enumerate the supported command-line options.'),
      longflags([help]),
      opt(help),
      shortflags([h]),
      type(boolean)
    ],
    [
      default(virtuoso),
      help('The endpoint that is used to store the LOD Washing Machine \c
            metadata.'),
      longflags([endpoint]),
      opt(endpoint),
      shortflags([e]),
      type(atom)
    ]
  ],
  opt_arguments(OptSpec, Options, _),
  
  % Process help.
  (   option(help(true), Options)
  ->  opt_help(OptSpec, Help),
      format(user_output, '~a\n', [Help]),
      halt
  ;   init(Options)
  ).

init(Options):-
  option(port(Port), Options),
  init_lle_settings(Port),
  start_app_server(Port).

