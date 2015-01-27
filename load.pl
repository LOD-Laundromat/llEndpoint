% The load file for the LOD Laundromat endpoint.
% part of the LOD Laundromat project.

:- use_module(library(ansi_term)).

:- multifile(user:project/3).
:- dynamic(user:project/3).
   user:project(llEndpoint, 'LOD Laundromat: Endpoint', lle).

:- use_module(load_project).
:- load_project([
     mt-'ModelTheory',
     plc-'Prolog-Library-Collection',
     plDcg,
     plGraph,
     plGraphDraw,
     plGraphViz,
     plHtml,
     plHttp,
     plLangTag,
     plLattice,
     plLatticeDraw,
     plRdf,
     plServer,
     plSet,
     plSvg,
     plTabular,
     plTms,
     plTree,
     plTreeDraw,
     plUri,
     plXml,
     plXsd
   ]).



:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- rdf_register_prefix(bnode, 'http://lodlaundromat.org/.well-known/genid/').
:- rdf_register_prefix(error, 'http://lodlaundromat.org/error/ontology/').
:- rdf_register_prefix(ll, 'http://lodlaundromat.org/resource/').
:- rdf_register_prefix(llo, 'http://lodlaundromat.org/ontology/').



:- use_module(library(process)).
%:- initialization(load_basket).
load_basket:-
  sleep(10),
  absolute_file_name(lle_basket('runBasket.sh'), File, [access(execute)]),
  file_directory_name(File, Directory),
  process_create(File, [], [cwd(Directory),detached(true)]).

