:- module(
  ll_web_progress_virtuoso,
  [
    ll_web_progress_virtuoso//0
  ]
).

/** <module> llEndpoint Web: Virtuoso progress

Web-based overview of ClioPatria progress.

@author Wouter Beek
@version 2014/09, 2014/11, 2015/01-2015/02
*/

:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(settings)).
:- use_module(library(uri)).

:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).

:- use_module(plHtml(html_pl_term)).

:- use_module(plServer(templates/menu_page)).

:- use_module(plSparql(query/sparql_query_api)).

:- use_module(plTabular(rdf_html_table)).





ll_web_progress_virtuoso -->
  pending_table,
  unpacking_table,
  unpacked_table_small,
  unpacked_table_medium,
  unpacked_table_large,
  cleaning_table.


%! pending_table// is det.

pending_table -->
  {lwm_sparql_select(
    [llo],
    [datadoc],
    [
      rdf(var(datadoc),llo:added,var(added)),
      not([rdf(var(datadoc),llo:startUnpack,var(start))])
    ],
    Rows,
    []
  )},
  progress_table(' pending data documents.', Rows).


%! unpacking_table(+Graph:atom)// is det.

unpacking_table -->
  {lwm_sparql_select(
    [llo],
    [datadoc],
    [
      rdf(var(datadoc),llo:startUnpack,var(startUnpack)),
      not([
        rdf(var(datadoc),llo:endUnpack,var(endUnpack))
      ])
    ],
    Rows,
    []
  )},
  progress_table(' data documents are being unpacked.', Rows).


%! unpacked_table_small// is det.

unpacked_table_small -->
  {
    Max is 0.5 * (1024 ** 3), % 0.5 GB
    build_unpacked_query(_, Max, Query),
    lwm_sparql_select([llo], [datadoc], Query, Rows, [])
  },
  progress_table(' SMALL unpacked data documents.', Rows).


%! unpacked_table_medium// is det.

unpacked_table_medium -->
  {
    Min is 0.5 * (1024 ** 3), % 0.5 GB
    Max is 2.5 * (1024 ** 3), % 2.5 GB
    build_unpacked_query(Min, Max, Query),
    lwm_sparql_select([llo], [datadoc], Query, Rows, [])
  },
  progress_table(' MEDIUM unpacked data documents.', Rows).


%! unpacked_table_large// is det.

unpacked_table_large -->
  {
    Min is 2.5 * (1024 ** 3), % 2.5 GB
    Max is 30 * (1024 ** 3), % 30 GB
    build_unpacked_query(Min, Max, Query),
    lwm_sparql_select([llo], [datadoc], Query, Rows, [])
  },
  progress_table(' LARGE unpacked data documents.', Rows).


%! cleaning_table// is det.

cleaning_table -->
  {lwm_sparql_select(
    [llo],
    [datadoc],
    [
      rdf(var(datadoc),llo:startClean,var(start_clean)),
      not([rdf(var(datadoc),llo:endClean,var(end_clean))])
    ],
    Rows,
    []
  )},
  progress_table(' data documents are being cleaned.', Rows).


%! cleaned_table// is det.

cleaned_table -->
  {lwm_sparql_select(
    [llo],
    [datadoc],
    [rdf(var(datadoc),llo:endClean,var(end_clean))],
    Rows,
    []
  )},
  progress_table(' cleaned data documents.', Rows).





% HELPERS %

%! build_unpacked_query(?Min:nonneg, ?Max:nonneg, -Query:atom) is det.
% Returns a query in which the given Min and Max integers denote
% the range restriction on the unpacked file size
% in terms of a SPARQL filter.

build_unpacked_query(Min, Max, Query2):-
  Query1 = [
    rdf(var(datadoc), llo:endUnpack, var(endUnpack)),
    not([
      rdf(var(datadoc), llo:startClean, var(startClean))
    ]),
    rdf(var(datadoc), llo:unpackedSize, var(unpackedSize))
  ],

  % Insert the range restriction on the unpacked file size as a filter.
  (   nonvar(Min)
  ->  MinFilter = >(var(unpackedSize),Min)
  ;   true
  ),
  (   nonvar(Max)
  ->  MaxFilter = <(var(unpackedSize),Max)
  ;   true
  ),
  exclude(var, [MinFilter,MaxFilter], FilterComponents),
  (   list_binary_term(FilterComponents, and, FilterContent)
  ->  append(Query1, [filter(FilterContent)], Query2)
  ;   Query2 = Query1
  ).



%! endpoint_options(
%!   +Options1:list(nvpair),
%!   -Endpoint:oneof([cliopatria,virtuoso_query]),
%!   -Options2:list(nvpair)
%! ) is det.

% Virtuoso queries are requested within a named graph.
endpoint_options(Options1, virtuoso_query, Options2):-
  lod_basket_graph(G1),
  lwm_version_graph(G2),
  merge_options([default_graph(G1),default_graph(G2)], Options1, Options2).



%! ll_authority(+Authortity:atom) is semidet.
%! ll_authority(-Authortity:atom) is det.

ll_authority('lodlaundromat.org').


%! ll_scheme(+Scheme:atom) is semidet.
%! ll_scheme(-Scheme:oneof([http])) is det.

ll_scheme(http).



%! lod_basket_graph(-Graph:atom) is det.

lod_basket_graph(Graph):-
  ll_scheme(Scheme),
  ll_authority(Authority),
  uri_components(Graph, uri_components(Scheme,Authority,'',_,seedlist)).



%! lwm_sparql_select(
%!   +Prefixes:list(atom),
%!   +Variables:list(atom),
%!   +Bgps:list(compound),
%!   -Result:list(list),
%!   +Options:list(nvpair)
%! ) is det.

lwm_sparql_select(Prefixes, Variables, Bgps, Result, Options1):-
  endpoint_options(Options1, Endpoint, Options2),
  loop_until_true(
    sparql_select(Endpoint, Prefixes, Variables, Bgps, Result, Options2)
  ).



%! lwm_version_graph(-Graph:atom) is det.

lwm_version_graph(Graph):-
  ll_scheme(Scheme),
  ll_authority(Authority),
  uri_components(
    Graph,
    uri_components(Scheme,Authority,'',_,'12')
  ).



%! progress_table(+CaptionPostfix:atom, +Rows:list(list(iri)))// is det.

progress_table(CaptionPostfix, Rows) -->
  {length(Rows, Length)},
  rdf_html_table(
		html([Length,CaptionPostfix]),
    [['Data document']|Rows],
    [
      header_column(true),
      header_row(true),
      indexed(true),
      maximum_number_of_rows(3)
    ]
  ).

