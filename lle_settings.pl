:- module(
  lle_settings,
  [
    lle_data_file/3, % +Md5:atom
                     % +Kind:oneof([clean,dirty])
                     % -Spec:compound
    lle_datadoc_directory/2, % +Md5:atom
                             % -Directory:atom
    lle_version_directory/1, % -VersionDirectory:atom
    lle_version_graph/1, % -Graph:iri
    lle_version_graph/2, % +Version:positive_integer
                         % -Graph:iri
    lle_version/1, % -Version:positive_integer
    lwm_debug_version/1 % -Version:positive_integer
  ]
).

/** <module> LOD Laudromat endpoint: settings

Settings for operating the LOD Laundromat endpoint.

@author Wouter Beek
@version 2014/08, 2015/01
*/

:- use_module(library(filesex)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(uri)).





%! lle_data_file(
%!   +Md5:atom,
%!   +Kind:oneof([clean,dirty]),
%!   -Spec:compound
%! ) is det.

lle_data_file(Md5, Kind, Spec):-
  lle_version(Version),
  lle_datadoc_directory(Md5, Md5Dir),
  lle_data_file_exists(Md5Dir, Kind, Name),
  atomic_list_concat([Version,Md5,Name], '/', RelativePath),
  Spec = data(RelativePath).

lle_data_file_exists(Md5Dir, Kind, TriplesName):-
  atomic_list_concat([Kind,nt,gz], '.', TriplesName),
  directory_file_path(Md5Dir, TriplesName, DataFile),
  exists_file(DataFile), !.
lle_data_file_exists(Md5Dir, Kind, QuadsName):-
  atomic_list_concat([Kind,nq,gz], '.', QuadsName),
  directory_file_path(Md5Dir, QuadsName, DataFile),
  exists_file(DataFile).


%! lle_datadoc_directory(+Md5:atom, -Directory:atom) is det.

lle_datadoc_directory(Md5, Md5Dir):-
  lle_version_directory(VersionDir),
  directory_file_path(VersionDir, Md5, Md5Dir).


lle_uri_components(uri_components(http,'lodlaundromat.org','','','')).


%! lle_version_directory(-VersionDirectory:atom) is det.

lle_version_directory(VersionDir):-
  % Place data documents in the data subdirectory.
  absolute_file_name(data(.), DataDir, [access(write),file_type(directory)]),

  % Add the LOD Washing Machine version to the directory path.
  lle_version(Version1),
  atom_number(Version2, Version1),
  directory_file_path(DataDir, Version2, VersionDir),
  make_directory_path(VersionDir).


%! lle_version_graph(-Graph:iri) is det.

lle_version_graph(Graph):-
  lle_version(Version),
  lle_version_graph(Version, Graph).

%! lle_version_graph(+Version:positive_integer, -Graph:iri) is det.

lle_version_graph(Version, Graph):-
  atom_number(Fragment, Version),
  lle_uri_components(uri_components(Scheme,Authority,_,_,_)),
  uri_components(Graph, uri_components(Scheme,Authority,_,_,Fragment)).


%! lle_version(+Version:positive_integer) is semidet.
%! lle_version(-Version:positive_integer) is det.

lle_version(11).


lwm_debug_version(11).


