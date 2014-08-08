:- module(
  lwm_web_deb,
  [
    lwm_deb/2, % +Request:list
                   % +HtmlStyle:atom
    serve_files_in_directory_with_cors/2 % +Alias:atom
                                         % +Request:list(nvpair)
  ]
).

/** <module> LOD laundry overview

Overview of the processing of the LOD Washing Machine,
intended for debugging purposes.

@author Wouter Beek
@version 2014/05-2014/06, 2014/08
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_session)). % HTTP session support.
:- use_module(library(semweb/rdf_db)).

:- use_module(generics(typecheck)).

:- use_module(plHtml(html_pl_term)).

:- use_module(plRdf_term(rdf_literal)).

:- use_module(plTabular(rdf_html_table)).
:- use_module(plTabular(rdf_html_table_pairs)).

:- use_module(lle(lle_settings)).

:- dynamic(url_md5_translation/2).



serve_files_in_directory_with_cors(Alias, Request):-
  cors_enable,
  serve_files_in_directory(Alias, Request).


lwm_deb(_, HtmlStyle):-
  Version = 11,
  lle_graph(Version, Graph),
  reply_html_page(
    HtmlStyle,
    title('LOD Laundromat'),
    html([
      h1('Overview of dissemination version'),
      \lwm_deb_version(Graph),
      h1('Overview of LWM status terms'),
      \lwm_status_terms(Graph)
    ])
  ).

%! lwm_deb_version(+Graph:atom)// is det.

lwm_deb_version(Graph) -->
  pending(Graph),
  unpacking(Graph),
  unpacked(Graph),
  cleaning(Graph),
  cleaned(Graph).

pending(Graph) -->
  {
    findall(
      Added-[Datadoc,Added],
      (
        rdf(Datadoc, ll:added, Added, Graph),
        \+ rdf(Datadoc, ll:start_unpack, _, Graph)
      ),
      Pairs
    ),
    length(Pairs, Length)
  },
  rdf_html_table_pairs(
    ['Data document','Added'],
    Pairs,
    html([\html_pl_term(lwm,Length),' pending data documents.']),
    [
      header_column(true),
      header_row(true),
      indexed(true),
      maximum_number_of_rows(10)
    ]
  ).

unpacking(Graph) -->
  {
    findall(
      StartUnpack2-[Datadoc,StartUnpack1],
      (
        rdf(Datadoc, ll:start_unpack, StartUnpack1, Graph),
        \+ rdf(Datadoc, ll:end_unpack, _, Graph),
        rdf_literal(StartUnpack1, StartUnpack2, _)
      ),
      Pairs
    ),
    length(Pairs, Length)
  },
  rdf_html_table_pairs(
    ['Data document','Unpacking start'],
    Pairs,
    html([\html_pl_term(lwm,Length),' data documents are being unpacked.']),
    [
      header_column(true),
      header_row(true),
      indexed(true),
      maximum_number_of_rows(10)
    ]
  ).

unpacked(Graph) -->
  {
    findall(
      EndUnpack-[Datadoc,EndUnpack],
      (
        rdf(Datadoc, ll:end_unpack, EndUnpack, Graph),
        \+ rdf(Datadoc, ll:start_clean, _, Graph)
      ),
      Pairs
    ),
    length(Pairs, Length)
  },
  rdf_html_table_pairs(
    ['Data document','Unpacking end'],
    Pairs,
    html([\html_pl_term(lwm,Length),' unpacked data documents.']),
    [
      header_column(true),
      header_row(true),
      indexed(true),
      maximum_number_of_rows(10)
    ]
  ).

cleaning(Graph) -->
  {
    findall(
      StartClean-[Datadoc,StartClean],
      (
        rdf(Datadoc, ll:start_clean, StartClean, Graph),
        \+ rdf(Datadoc, ll:end_clean, _, Graph)
      ),
      Pairs
    ),
    length(Pairs, Length)
  },
  rdf_html_table_pairs(
    ['Data document','Cleaning start'],
    Pairs,
    html([\html_pl_term(lwm,Length),' data documents are being cleaned.']),
    [
      header_column(true),
      header_row(true),
      indexed(true),
      maximum_number_of_rows(10)
    ]
  ).

cleaned(Graph) -->
  {
    findall(
      EndClean-[Datadoc,EndClean],
      (
        rdf(Datadoc, ll:end_clean, EndClean, Graph)
      ),
      Pairs
    ),
    length(Pairs, Length)
  },
  rdf_html_table_pairs(
    ['Data document','Cleaning end'],
    Pairs,
    html([\html_pl_term(lwm,Length),' cleaned data documents.']),
    [
      header_column(true),
      header_row(true),
      indexed(true),
      maximum_number_of_rows(10)
    ]
  ).

lwm_status_terms(Graph) -->
  {
    findall(
      Status2-Datadoc,
      (
        rdf(Datadoc, ll:status, Status1, Graph),
        status_to_atom(Status1, Status2)
      ),
      Pairs1
    ),
    keysort(Pairs1, Pairs2),
    group_pairs_by_key(Pairs2, Pairs3)
  },
  lwm_status_tables(Graph, Pairs3).

lwm_status_tables(_, []) --> [].
lwm_status_tables(Graph, [Status-Datadocs|T]) -->
  lwm_status_table(Graph, Status, Datadocs),
  lwm_status_tables(Graph, T).

lwm_status_table(Graph, Status, Datadocs1) -->
  {
    sort(Datadocs1, Datadocs2),
    maplist(datadoc_to_row(Graph), Datadocs2, Rows),
    length(Datadocs2, Length)
  },
  rdf_html_table(
    html([
      \html_pl_term(lwm,Length),
      ' data documents with status ',
      \html_pl_term(lwm,Status)
    ]),
    [['Datadoc','Dirty URL']|Rows],
    [graph(Graph),header_row(true)]
  ).

datadoc_to_row(Graph, Datadoc, [Datadoc,Url]):-
  rdf(Datadoc, ll:url, Url, Graph).

status_to_atom(literal(type(XsdString,Status1)), Status2):-
  rdf_equal(xsd:string, XsdString),
  read_term_from_atom(Status1, StatusTerm, []),
  once(status_to_atom0(StatusTerm, Status2)), !.
status_to_atom(Status, Status).

status_to_atom0(exception(error(existence_error(directory,_),_)), existence_error_directory).
status_to_atom0(exception(error(existence_error(source_sink,_),_)), existence_error_source_sink).
status_to_atom0(exception(error(http_status(Status),_)), Label):-
  atomic_list_concat([http_status,Status], '_', Label).
status_to_atom0(exception(error(instantiation_error,_)), instantiation_error).
status_to_atom0(exception(error(io_error(Mode,_),_)), Label):-
  atomic_list_concat([io_error,Mode], '_', Label).
status_to_atom0(exception(error(limit_exceeded(max_errors,_),_)), limit_exceeded_max_errors).
status_to_atom0(exception(error(no_rdf(_))), no_rdf).
status_to_atom0(exception(error(socket_error(_),_)), socket_error).
status_to_atom0(exception(error(ssl_error(Kind),_)), Label):-
  atomic_list_concat([ssl_error,Kind], '_', Label).
status_to_atom0(exception(error(timeout_error(Mode,_),_)), Label):-
  atomic_list_concat([timeout_error,Mode], '_', Label).
status_to_atom0(exception(error(type_error(xml_dom,_),_)), type_error_xml_dom).
status_to_atom0(false, false).
status_to_atom0(true, true).

