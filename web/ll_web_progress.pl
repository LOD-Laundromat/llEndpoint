:- module(ll_web_progress, []).

/** <module> LOD Washing Machine: Progress

A Web-based debug tool for tracking the progress of the LOD Washing Machine.

@author Wouter Beek
@version 2014/05-2014/06, 2014/08-2014/09, 2015/01-2015/02
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(math(float_ext)).

:- use_module(plHtml(html_pl_term)).

:- use_module(plRdf(term/rdf_literal)).

:- use_module(plTabular(rdf_html_table_pairs)).

:- use_module(lle(web/ll_web_generics)).

:- http_handler(lle(progress), ll_web_progress, [priority(1)]).





% Overview of LOD Washing Machine progress.
ll_web_progress(_):-
  user:current_html_style(HtmlStyle),
  reply_html_page(
    HtmlStyle,
    title('LOD Laundromat'),
    \lle_body([
      \pending_table,
      \unpacking_table,
      \unpacked_table(_, 0.5, 'SMALL'),
      \unpacked_table(0.5, 2.5, 'MEDIUM'),
      \unpacked_table(2.5, 30, 'LARGE'),
      \cleaning_table,
      \cleaned_table
    ])
  ).


%! pending_table// is det.

pending_table -->
  {
    findall(
      Added-[Datadoc,Added],
      (
        rdf_has(Datadoc, llo:added, Added),
        \+ rdf_has(Datadoc, llo:startUnpack, _)
      ),
      Rows
    )
  },
  progress_table(' pending data documents.', 'Added', Rows).


%! unpacking_table// is det.

unpacking_table -->
  {
    findall(
      StartUnpack2-[Datadoc,StartUnpack1],
      (
        rdf_has(Datadoc, llo:startUnpack, StartUnpack1),
        \+ rdf_has(Datadoc, llo:endUnpack, _),
        rdf_literal_data(value, StartUnpack1, StartUnpack2)
      ),
      Rows
    )
  },
  progress_table(
    ' data documents are being unpacked.',
    'Unpacking start',
    Rows
  ).


%! unpacked_table(?Min:float, ?Max:float, +Category:atom)// is det.

unpacked_table(Min0, Max0, Category) -->
  {
    Min is Min0 * (1024 ** 3),
    Max is Max0 * (1024 ** 3),
    findall(
      EndUnpack-[Datadoc,EndUnpack],
      (
        rdf_has(Datadoc, llo:endUnpack, EndUnpack),
        \+ rdf_has(Datadoc, llo:startClean, _),
        rdf_typed_literal(
          Datadoc,
          llo:unpackedSize,
          xsd:nonNegativeInteger,
          UnpackedSize
        ),
        between_float(Min, Max, UnpackedSize)
      ),
      Pairs
    ),
    atomic_list_concat([' unpacked',Category,'data documents.'], ' ', Caption)
  },
  progress_table(Caption, 'Unpacking end', Pairs).


%! cleaning_table// is det.

cleaning_table -->
  {
    findall(
      StartClean-[Datadoc,StartClean],
      (
        rdf_has(Datadoc, llo:startClean, StartClean),
        \+ rdf_has(Datadoc, llo:endClean, _)
      ),
      Pairs
    )
  },
  progress_table(
    ' data documents are being cleaned.',
    'Cleaning start',
    Pairs
  ).


%! cleaned_table// is det.

cleaned_table -->
  {
    findall(
      EndClean-[Datadoc,EndClean],
      rdf_has(Datadoc, llo:endClean, EndClean),
      Pairs
    )
  },
  progress_table(' cleaned data documents.', 'Cleaning end', Pairs).





% HELPERS %

%! progress_table(
%!   +CaptionPostfix:atom,
%!   +ColumnHeader:atom,
%!   +Pairs:list(pair)
%! )// is det.

progress_table(CaptionPostfix, ColumnHeader, Pairs1) -->
  {
    keysort(Pairs1, Pairs2),
    length(Pairs2, Length)
  },
  rdf_html_table_pairs(
    ['Data document',ColumnHeader],
    Pairs2,
    html([\html_pl_term(lwm,Length),CaptionPostfix]),
    [
      header_column(true),
      header_row(true),
      indexed(true),
      maximum_number_of_rows(3)
    ]
  ).

