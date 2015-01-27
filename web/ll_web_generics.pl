:- module(
  ll_web_generics,
  [
    lle_body//1 % :Content
  ]
).

/** <module> LOD Washing Machine: debug tool

Overview of the processing of the LOD Washing Machine,
intended for debugging purposes.

@author Wouter Beek
@version 2014/05-2014/06, 2014/08, 2015/01
*/

:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).

:- html_meta(lle_body(html,?,?)).





lle_body(Content) -->
  html([
    \html_requires(css(pure)),
    div([style='margin-left: 1.5cm; margin-top: 0.5cm;'], Content)
  ]).

