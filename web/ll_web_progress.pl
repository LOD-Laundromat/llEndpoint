:- module(ll_web_progress, []).

/** <module> LOD Washing Machine: Progress

A Web-based debug tool for tracking the progress of the LOD Washing Machine.

@author Wouter Beek
@version 2014/05-2014/06, 2014/08-2014/09, 2015/01-2015/03
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(settings)).

:- use_module(plHtml(html)).

:- use_module(lle(lle_settings)).
:- use_module(lle(web/ll_web_generics)).
:- use_module(lle(web/ll_web_progress_cliopatria)).
:- use_module(lle(web/ll_web_progress_virtuoso)).

:- http_handler(lle(progress), ll_web_progress, [priority(1)]).





% Overview of LOD Washing Machine progress.
ll_web_progress(_):-
  setting(lle_settings:endpoint, Endpoint),
  reply_styled_html_page(
    title('LOD Laundromat'),
    \lle_body(\ll_web_progress0(Endpoint))
  ).

ll_web_progress0(both) -->
  html([
    h1('ClioPatria'),
    \ll_web_progress0(cliopatria),
    h1('Virtuoso'),
    \ll_web_progress0(virtuoso)
  ]).

ll_web_progress0(cliopatria) -->
  ll_web_progress_cliopatria.
ll_web_progress0(virtuoso) -->
  ll_web_progress_virtuoso.

