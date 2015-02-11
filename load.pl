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
     plRdfDraw,
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



% Register RDF prefixes.
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- rdf_register_prefix(bnode, 'http://lodlaundromat.org/.well-known/genid/').
:- rdf_register_prefix(error, 'http://lodlaundromat.org/error/ontology/').
:- rdf_register_prefix(ll, 'http://lodlaundromat.org/resource/').
:- rdf_register_prefix(llo, 'http://lodlaundromat.org/ontology/').



% Load generic resources for Web pages.
:- use_module(plServer(templates/template_generics)).



% Register HTTP handlers.
:- use_module(plTabular(rdf_tabular)).
:- use_module(lle(lle_data)).
:- use_module(lle(lle_reset)).
:- use_module(lle(basket/basket_web)).
:- use_module(lle(web/ll_web_errors)).
:- use_module(lle(web/ll_web_progress)).

