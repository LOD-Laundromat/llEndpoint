:- module(
  lle_data,
  [
    clean_data/1 % +Request:list(nvpair)
  ]
).

/** <module> LOD Laundromat: data

Serving of data cleaned by the LOD Washing Machine.

@author Wouter Beek
@version 2014/08
*/

:- use_module(library(http/http_cors)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_session)). % HTTP session support.

:- use_module(generics(request_ext)).
:- use_module(generics(uri_query)).

:- use_module(lle(lle_settings)).



clean_data(Request):-
  data_md5(Request, Md5),
  data_kind(Request, Kind),
  lle_data_file(Md5, Kind, File),
  http_reply_file(File, [], Request).



%! data_kind(+Request:list(nvpair), -Kind:oneof([clean,dirty])) is det.

data_kind(Request, Kind):-
  request_query_nvpair(Request, kind, Kind), !.
data_kind(_, clean).


%! data_md5(+Request:list(nvpair), -Md5:atom) is det.

data_md5(Request, Md5):-
  memberchk(path(Path), Request),
  atomic_list_concat(['',data,Md5], '/', Path), !.
data_md5(Request, Md5):-
  request_query_nvpair(Request, md5, Md5).
