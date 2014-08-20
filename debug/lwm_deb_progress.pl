:- module(
  lwm_deb_progress,
  [
    lwm_deb_progress/2 % +Request:list(nvpair)
                         % +HtmlStyle
  ]
).

/** <module> LOD Washing Machine: Progress

A Web-based debug tool for tracking the progress of the LOD Washing Machine.

@author Wouter Beek
@version 2014/05-2014/06, 2014/08
*/

:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).

:- use_module(generics(uri_search)).

:- use_module(plHtml(html_pl_term)).

:- use_module(plRdf_term(rdf_literal)).

:- use_module(plTabular(rdf_html_table_pairs)).

:- use_module(lle(lle_settings)).



%! lwm_deb_progress(+Request:list(nvpair), +HtmlStyle)// is det.

lwm_deb_progress(Request, HtmlStyle):-
  lwm_debug_version(DefaultVersion),
  request_search_read(Request, version, Version, DefaultVersion),
  lle_version_graph(Version, Graph),
  reply_html_page(
    HtmlStyle,
    title('LOD Laundromat'),
    html([
      \pending_table(Graph),
      \unpacking_table(Graph),
      \unpacked_table(Graph),
      \cleaning_table(Graph),
      \cleaned_table(Graph)
    ])
  ).


%! pending_table(+Graph:atom)// is det.

pending_table(Graph) -->
  {
    findall(
      Added-[Datadoc,Added],
      (
        rdf(Datadoc, ll:added, Added, Graph),
        \+ rdf(Datadoc, ll:start_unpack, _, Graph)
      ),
      Pairs
    )
  },
  progress_table(' pending data documents.', 'Added', Pairs).


%! unpacking_table(+Graph:atom)// is det.

unpacking_table(Graph) -->
  {
    findall(
      StartUnpack2-[Datadoc,StartUnpack1],
      (
        rdf(Datadoc, ll:start_unpack, StartUnpack1, Graph),
        \+ rdf(Datadoc, ll:end_unpack, _, Graph),
        rdf_literal(StartUnpack1, StartUnpack2, _)
      ),
      Pairs
    )
  },
  progress_table(
    ' data documents are being unpacked.',
    'Unpacking start',
    Pairs
  ).


%! unpacked_table(+Graph:atom)// is det.

unpacked_table(Graph) -->
  {
    findall(
      EndUnpack-[Datadoc,EndUnpack],
      (
        rdf(Datadoc, ll:end_unpack, EndUnpack, Graph),
        \+ rdf(Datadoc, ll:start_clean, _, Graph)
      ),
      Pairs
    )
  },
  progress_table(' unpacked data documents.', 'Unpacking end', Pairs).


%! cleaning_table(+Graph:atom)// is det.

cleaning_table(Graph) -->
  {findall(
    StartClean-[Datadoc,StartClean],
    (
      rdf(Datadoc, ll:start_clean, StartClean, Graph),
      \+ rdf(Datadoc, ll:end_clean, _, Graph)
    ),
    Pairs
  )},
  progress_table(
    ' data documents are being cleaned.',
    'Cleaning start',
    Pairs
  ).


%! cleaned_table(+Graph:atom)// is det.

cleaned_table(Graph) -->
  {findall(
    EndClean-[Datadoc,EndClean],
    rdf(Datadoc, ll:end_clean, EndClean, Graph),
    Pairs
  )},
  progress_table(' cleaned data documents.', 'Cleaning end', Pairs).



% Helpers

%! progress_table(
%!   +CaptionPostfix:atom,
%!   +ColumnHeader:atom,
%!   +Pairs:list(pair)
%! )// is det.

progress_table(CaptionPostfix, ColumnHeader, Pairs) -->
  {length(Pairs, Length)},
  rdf_html_table_pairs(
    ['Data document',ColumnHeader],
    Pairs,
    html([\html_pl_term(lwm,Length),CaptionPostfix]),
    [
      header_column(true),
      header_row(true),
      indexed(true),
      maximum_number_of_rows(10)
    ]
  ).
