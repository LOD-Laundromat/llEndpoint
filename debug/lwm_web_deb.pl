:- module(
  lwm_web_deb,
  [
    lwm_web_deb/2, % +Request:list
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

:- use_module(plTabular(rdf_html_table_pairs)).

:- use_module(lle(lle_settings)).

:- dynamic(url_md5_translation/2).



serve_files_in_directory_with_cors(Alias, Request):-
  cors_enable,
  serve_files_in_directory(Alias, Request).


lwm_web_deb(_, HtmlStyle):-
  reply_html_page(
    HtmlStyle,
    title('LOD Laundromat'),
    html([
      h1('Overview of dissemination version'),
      \lwm_web_deb_mode
    ])
  ).

%! lwm_web_deb_mode// is det.

lwm_web_deb_mode -->
  {lle_graph(Graph)},
  lwm_web_deb0(Graph).

%! lwm_web_deb_mode(+Version:positive_integer)// is det.

lwm_web_deb(Version) -->
  {lle_graph(Version, Graph)},
  lwm_web_deb0(Graph).

lwm_web_deb0(Graph) -->
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

