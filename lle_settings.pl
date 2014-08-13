:- module(
  lle_settings,
  [
    lle_clean_file/2, % +Md5:atom
                      % -File:atom
    lle_datadoc_directory/2, % +Md5:atom
                             % -Directory:atom
    lle_version_directory/1, % -Directory:atom
    lle_version_number/1, % -VersionNumber:positive_integer
    lle_version_object/1, % -Version:iri
    lle_version_object/2 % +VersionNumber:positive_integer
                         % -Version:iri
  ]
).

/** <module> LOD Laudromat endpoint: settings

Settings for operating the LOD Laundromat endpoint.

@author Wouter Beek
@version 2014/08
*/

:- use_module(library(filesex)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- rdf_register_prefix(ll, 'http://lodlaundromat.org/vocab#').



%! lle_clean_file(+Md5:atom, -File:atom) is det.

lle_clean_file(Md5, CleanFile):-
  lle_datadoc_directory(Md5, Md5Dir),
  (
    directory_file_path(Md5Dir, 'clean.nt.gz', CleanFile),
    exists_file(CleanFile), !
  ;
    directory_file_path(Md5Dir, 'clean.nq.gz', CleanFile),
    exists_file(CleanFile)
  ).


%! lle_datadoc_directory(+Md5:atom, -Directory:atom) is det.

lle_datadoc_directory(Md5, Md5Dir):-
  lle_version_directory(DataDir),
  directory_file_path(DataDir, Md5, Md5Dir).


%! lle_version_directory(-Directory:atom) is det.

lle_version_directory(VersionDir):-
  lle_version_object(Version),
  rdf(Version, ll:datadir, VersionUri),
  uri_file_name(VersionUri, VersionDir).


lle_uri_components(uri_components(http,'lodlaundromat.org','','','')).


%! lle_version_number(+VersionNumber:positive_integer) is semidet.
%! lle_version_number(-VersionNumber:positive_integer) is det.

lle_version_number(10).


%! lle_version_object(-Version:iri) is det.

lle_version_object(Version):-
  lle_version_number(VersionNumber),
  lle_version_object(VersionNumber, Version).

%! lle_version_object(+VersionNumber:positive_integer, -Version:iri) is det.

lle_version_object(VersionNumber, Version):-
  atom_number(Fragment, VersionNumber),
  lle_uri_components(uri_components(Scheme,Authority,_,_,_)),
  uri_components(Version, uri_components(Scheme,Authority,_,_,Fragment)).

