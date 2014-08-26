:- module(
  lwm_deb_exceptions,
  [
    lwm_deb_exceptions/2 % +Request:list(nvpair)
                         % +HtmlStyle
  ]
).

/** <module> LOD Washing Machine: Exceptions

A Web-based debug tool for surveying exceptions thrown
by the LOD Washing Machine.

@author Wouter Beek
@version 2014/08
*/

:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).

:- use_module(generics(request_ext)).
:- use_module(generics(uri_query)).

:- use_module(plHtml(html_pl_term)).

:- use_module(plTabular(rdf_html_table)).

:- use_module(lle(lle_settings)).



%! lwm_deb_exceptions(+Request:list(nvpair), +HtmlStyle)// is det.

lwm_deb_exceptions(Request, HtmlStyle):-
  lwm_debug_version(DefaultVersion),
  request_query_nvpair(Request, version, Version, DefaultVersion),
  lle_version_graph(Version, Graph),
  reply_html_page(
    HtmlStyle,
    title('LOD Laundromat'),
    html(\lwm_deb_exceptions(Graph))
  ).


lwm_deb_exceptions(Graph) -->
  {
    findall(
      Exception2-Datadoc,
      (
        rdf(Datadoc, ll:exception, Exception1, Graph),
        exception_to_atom(Exception1, Exception2)
      ),
      Pairs1
    ),
    keysort(Pairs1, Pairs2),
    group_pairs_by_key(Pairs2, Pairs3)
  },
  lwm_exception_tables(Graph, Pairs3).


lwm_exception_table(Graph, Exception, Datadocs1) -->
  {
    sort(Datadocs1, Datadocs2),
    maplist(datadoc_to_row(Graph), Datadocs2, Rows),
    length(Datadocs2, Length)
  },
  rdf_html_table(
    html([
      \html_pl_term(lwm,Length),
      ' data documents with exception ',
      \html_pl_term(lwm,Exception)
    ]),
    [['Datadoc','URL','Path']|Rows],
    [graph(Graph),header_row(true),maximum_number_of_rows(10)]
  ).

lwm_exception_tables(_, []) --> [].
lwm_exception_tables(Graph, [Exception-Datadocs|T]) -->
  lwm_exception_table(Graph, Exception, Datadocs),
  lwm_exception_tables(Graph, T).


%! datadoc_source(+Graph:atom, +Datadoc:iri, -Source:list) is det.

datadoc_source(Graph, Datadoc, [Url]):-
  rdf(Datadoc, ll:url, Url, Graph), !.
datadoc_source(Graph, Datadoc, Source):-
  datadoc_source0(Graph, Datadoc, Source0),
  reverse(Source0, Source).

datadoc_source0(Graph, Datadoc, [Path|T]):-
  rdf(Datadoc, ll:path, literal(type(xsd:string,Path)), Graph),
  rdf(Parentdoc, ll:contains_entry, Datadoc, Graph),
  datadoc_source(Graph, Parentdoc, T).


%! datadoc_to_row(+Graph:atom, +Datadoc:iri, -Row:list) is det.

datadoc_to_row(Graph, Datadoc, [Datadoc,Url,Path]):-
  datadoc_source(Graph, Datadoc, [Url|Paths]),
  atomic_list_concat(Paths, '-', Path).


%! exception_to_atom(+Exception, -Atom) is det.

exception_to_atom(literal(type(XsdString,Status1)), Status2):-
  rdf_equal(xsd:string, XsdString),
  read_term_from_atom(Status1, StatusTerm, []),
  once(exception_to_atom0(StatusTerm, Status2)), !.
exception_to_atom(Status, Status).

exception_to_atom0(exception(error(permission_error(redirect,_,_),_)), permission_error(rediction_loop)).
exception_to_atom0(exception(error(existence_error(directory,_),_)), existence_error(directory)).
exception_to_atom0(exception(error(existence_error(file,_),_)), existence_error(file)).
exception_to_atom0(exception(error(existence_error(source_sink,_),_)), existence_error(source_sink)).
exception_to_atom0(exception(error(http_status(Status),_)), Label):-
  atomic_list_concat([http_status,Status], '_', Label).
exception_to_atom0(exception(error(instantiation_error(_),_)), instantiation_error).
exception_to_atom0(exception(error(io_error(Mode,_),_)), Label):-
  atomic_list_concat([io_error,Mode], '_', Label).
exception_to_atom0(exception(error(limit_exceeded(max_errors,_),_)), limit_exceeded_max_errors).
exception_to_atom0(exception(error(no_rdf(_))), no_rdf).
exception_to_atom0(exception(error(socket_error(_),_)), socket_error).
exception_to_atom0(exception(error(ssl_error(Kind),_)), Label):-
  atomic_list_concat([ssl_error,Kind], '_', Label).
exception_to_atom0(exception(error(timeout_error(Mode,_),_)), Label):-
  atomic_list_concat([timeout_error,Mode], '_', Label).
exception_to_atom0(exception(error(type_error(xml_dom,_),_)), type_error_xml_dom).
exception_to_atom0(false, false).
exception_to_atom0(true, true).

