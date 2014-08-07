% The load file for the llEndpoint project.

:- use_module(library(ansi_term)).

:- multifile(user:project/3).
:- dynamic(user:project/3).
   user:project(llEndpoint, 'LOD Laundromat: Endpoint', lle).

:- use_module(load_project).
:- load_project([
    plc-'Prolog-Library-Collection',
    plGraph,
    %plGraphViz,
    plHtml,
    plRdf,
    plServer,
    plTabular
]).

