:- module(
  lwm_deb,
  [
    lwm_deb/2 % +Request:list
              % +HtmlStyle:atom

  ]
).

/** <module> LOD laundry: Washing Machine debug tool

Overview of the processing of the LOD Washing Machine,
intended for debugging purposes.

@author Wouter Beek
@version 2014/05-2014/06, 2014/08
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_session)). % HTTP session support.

:- use_module(plHtml(html)).
:- use_module(plHtml(html_list)).



lwm_deb(_, HtmlStyle):-
  reply_html_page(
    HtmlStyle,
    title('LOD Laundromat - LOD Washing Machine debug tools'),
    html([
      'LOD Washing Machine debug tools:',
      \html_list(
        [lwm_deb_errors,lwm_deb_progress],
        html_http_handler,
        [ordered(false)]
      )
    ])
  ).

