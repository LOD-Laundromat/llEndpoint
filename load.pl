% The load file for the llEndpoint project.

:- use_module(library(ansi_term)).

:- multifile(user:project/3).
:- dynamic(user:project/3).
   user:project(llEndpoint, 'LOD Laundromat: Endpoint', lle).

:- use_module(load_project).
:- load_project([
    plc-'Prolog-Library-Collection',
    plDcg,
    plGraph,
    plHtml,
    plXsd,
    plRdf,
    plServer,
    plTabular
]).



:- use_module(library(process)).

%:- initialization(load_basket).

load_basket:-
  sleep(10),
  absolute_file_name(lle_basket('runBasket.sh'), File, [access(execute)]),
  file_directory_name(File, Directory),
  process_create(File, [], [cwd(Directory),detached(true)]).

