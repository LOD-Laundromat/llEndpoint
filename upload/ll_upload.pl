:- module(ll_upload, []).

:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(http/http_open)).
:- use_module(library(uri)).

:- initialization(init).

init:-
  absolute_file_name(dbpedia_seedpoints, File, [access(read),extensions([csv])]),
  csv_read_file(File, Rows),
  maplist(upload, Rows).

upload(row(Url)):-
  uri_query_components(Query, [url=Url]),
  uri_components(
    Uri,
    uri_components(http,'backend.lodlaundromat.org','/',Query,_)
  ),
  setup_call_cleanup(
    http_open(Uri, Stream, [status_code(Status)]),
    writeln(Status),
    close(Stream)
  ).

