:- module(ll_web_errors, []).

/** <module> LOD Washing Machine: Exceptions

A Web-based debug tool for surveying exceptions thrown
by the LOD Washing Machine.

@author Wouter Beek
@version 2014/08-2014/09, 2015/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdfs)).

:- use_module(generics(list_ext)).

:- use_module(plHtml(html_pl_term)).

:- use_module(plTabular(rdf_html_table)).

:- use_module(lle(web/ll_web_generics)).

:- http_handler(lle(errors), ll_web_errors, [priority(1)]).





%! ll_web_errors(+Request:list(nvpair), +HtmlStyle)// is det.

ll_web_errors(_):-
  user:current_html_style(HtmlStyle),
  reply_html_page(
    HtmlStyle,
    title('LOD Laundromat'),
    \lle_body(\ll_web_errors)
  ).


ll_web_errors -->
  {
    % The aggregation prevents the same MD5 from
    % showing up multiple times.
    aggregate_all(
      set(Error2-Datadoc),
      (
        (   rdf_has(Datadoc, llo:exception, Error1)
        ;   rdf_has(Datadoc, llo:warning, Error1)
        ),
        once(exception_to_atom(Error1, Error2))
      ),
      Pairs1
    ),
    keysort(Pairs1, Pairs2),
    group_pairs_by_key(Pairs2, Pairs3)
  },
  lwm_exception_tables(Pairs3).


lwm_exception_table(Exception, Datadocs1) -->
  {
    length(Datadocs1, Length),
    list_truncate(Datadocs1, 3, Datadocs2),
    sort(Datadocs2, Datadocs3),
    maplist(datadoc_to_row, Datadocs3, Rows)
  },
  rdf_html_table(
    html([
      \html_pl_term(lwm,Length),
      ' data documents with exception ',
      \html_pl_term(lwm,Exception)
    ]),
    [['Datadoc','URL','Path']|Rows],
    [header_row(true)]
  ).

lwm_exception_tables([]) --> [].
lwm_exception_tables([Exception-Datadocs|T]) -->
  lwm_exception_table(Exception, Datadocs),
  lwm_exception_tables(T).


%! datadoc_source(+Datadoc:iri, -Source:list) is det.

datadoc_source(Datadoc, [Url]):-
  rdf_has(Datadoc, llo:url, Url), !.
datadoc_source(Datadoc, Source):-
  datadoc_source0(Datadoc, Source0), !,
  reverse(Source0, Source).
datadoc_source(_, ['FAILURE','FAILURE']).

datadoc_source0(Datadoc, [Path|T]):-
  rdf_has(Datadoc, llo:path, literal(type(xsd:string,Path))),
  rdf_has(Parentdoc, llo:containsEntry, Datadoc),
  datadoc_source(Parentdoc, T).


%! datadoc_to_row(+Datadoc:iri, -Row:list) is det.

datadoc_to_row(Datadoc, [Datadoc,Url,Path]):-
  datadoc_source(Datadoc, [Url|Paths]),
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

