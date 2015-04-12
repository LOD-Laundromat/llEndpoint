% Standalone startup file for the LOD Laundromat endpoint.

% Load modules.
:- if(current_prolog_flag(argv, ['--debug'])).
  :- ensure_loaded(debug).
:- else.
  :- ensure_loaded(load).
:- endif.


:- use_module(plc(server/app_server)).

:- initialization(init).

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
      default(virtuoso),
      help('The endpoint that is used to store the LOD Washing Machine \c
            metadata.'),
      longflags([endpoint]),
      opt(endpoint),
      shortflags([e]),
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
      default(3040),
      help('The port at which the triple store for the scrape metadata \c
            can be reached.'),
      longflags([port]),
      opt(port),
      shortflags([p]),
      type(integer)
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
  start_app_server([port(Port)]).

