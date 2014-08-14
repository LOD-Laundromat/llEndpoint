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



clean_data(Request):-
gtrace,
  % Kind.
  (
    request_search_read(Request, kind, Kind), !
  ;
    Kind = clean
  ),
  
  % MD5.
  request_search_read(Request, md5, Md5),
  
  lle_data_file(Md5, Kind, DataFile),
  http_reply_file(DataFile, [], Request).

