:- module(lwm_deb, []).

/** <module> LOD Washing Machine: debug tool

Overview of the processing of the LOD Washing Machine,
intended for debugging purposes.

@author Wouter Beek
@version 2014/05-2014/06, 2014/08, 2015/01
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_session)). % HTTP session support.

:- use_module(plHtml(html)).
:- use_module(plHtml(html_list)).

:- http_handler(root(lwm), lwm_deb, [id(lwm)]).





lwm_deb(_):-
  user:current_html_style(HtmlStyle),
  reply_html_page(
    HtmlStyle,
    title('LOD Washing Machine - Debug tools'),
    html([
      'Debug tools for the LOD Washing Machine:',
      \html_list(
        [lwm_deb_errors,lwm_deb_progress],
        html_http_handler,
        [ordered(false)]
      )
    ])
  ).

