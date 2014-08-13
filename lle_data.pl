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

:- use_module(generics(uri_search)).

:- use_module(lle(lle_settings)).

:- dynamic(http:location/3).
:- multifile(http:location/3).
   http:location(lle_data, root(data), []).



clean_data(Request):-
  request_search_read(Request, md5, Md5),
  lle_clean_file(Md5, File),
  http_reply_file(File, [], Request).

