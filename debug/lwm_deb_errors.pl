:- module(lwm_deb_errors, []).

/** <module> LOD Washing Machine: Exceptions

A Web-based debug tool for surveying exceptions thrown
by the LOD Washing Machine.

@author Wouter Beek
@version 2014/08-2014/09, 2015/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdfs)).

:- use_module(generics(list_ext)).

:- use_module(plHttp(request_ext)).

:- use_module(plHtml(html_pl_term)).

:- use_module(plTabular(rdf_html_table)).

:- use_module(lle(lle_settings)).

:- http_handler(lle(errors), lwm_deb_errors, [priority(1)]).





%! lwm_deb_errors(+Request:list(nvpair), +HtmlStyle)// is det.

lwm_deb_errors(Request):-
  lwm_debug_version(DefaultVersion),
  request_query_nvpair(Request, version, Version, DefaultVersion),
  lle_version_graph(Version, Graph),
  reply_html_page(
    title('LOD Laundromat'),
    html(\lwm_deb_errors(Graph))
  ).


lwm_deb_errors(Graph) -->
  {
    % The of aggregation prevents the same MD5 from
    % showing up multiple times.
    aggregate_all(
      set(Error2-Datadoc),
      (
        (
          rdf(Datadoc, llo:exception, Error1, Graph)
        ;
          rdf(Datadoc, llo:warning, Error1, Graph)
        ),
        once(exception_to_atom(Error1, Error2))
      ),
      Pairs1
    ),
    keysort(Pairs1, Pairs2),
    group_pairs_by_key(Pairs2, Pairs3)
  },
  lwm_exception_tables(Graph, Pairs3).


lwm_exception_table(Graph, Exception, Datadocs1) -->
  {
    length(Datadocs1, Length),
    list_truncate(Datadocs1, 3, Datadocs2),
    sort(Datadocs2, Datadocs3),
    maplist(datadoc_to_row(Graph), Datadocs3, Rows)
  },
  rdf_html_table(
    html([
      \html_pl_term(lwm,Length),
      ' data documents with exception ',
      \html_pl_term(lwm,Exception)
    ]),
    [['Datadoc','URL','Path']|Rows],
    [graph(Graph),header_row(true)]
  ).

lwm_exception_tables(_, []) --> [].
lwm_exception_tables(Graph, [Exception-Datadocs|T]) -->
  lwm_exception_table(Graph, Exception, Datadocs),
  lwm_exception_tables(Graph, T).


%! datadoc_source(+Graph:atom, +Datadoc:iri, -Source:list) is det.

datadoc_source(Graph, Datadoc, [Url]):-
  rdf(Datadoc, llo:url, Url, Graph), !.
datadoc_source(Graph, Datadoc, Source):-
  datadoc_source0(Graph, Datadoc, Source0), !,
  reverse(Source0, Source).
datadoc_source(_, _, ['FAILURE','FAILURE']).

datadoc_source0(Graph, Datadoc, [Path|T]):-
  rdf(Datadoc, llo:path, literal(type(xsd:string,Path)), Graph),
  rdf(Parentdoc, llo:containsEntry, Datadoc, Graph),
  datadoc_source(Graph, Parentdoc, T).


%! datadoc_to_row(+Graph:atom, +Datadoc:iri, -Row:list) is det.

datadoc_to_row(Graph, Datadoc, [Datadoc,Url,Path]):-
  datadoc_source(Graph, Datadoc, [Url|Paths]),
  atomic_list_concat(Paths, '-', Path).


%! exception_to_atom(+Term, -Atom) is det.

exception_to_atom(BNode, Atom):-
  llo_is_bnode(BNode), !,
  rdf(BNode, rdf:type, Class),
  (   rdfs_label(Class, Atom0)
  ->  Atom = Atom0
  ;   Atom = Class
  ).
exception_to_atom(Resource, Atom):-
  (   rdfs_label(Resource, Atom0)
  ->  Atom = Atom0
  ;   Atom = Resource
  ).

llo_is_bnode(BNode):-
  rdf_global_id(bnode:_, BNode).

