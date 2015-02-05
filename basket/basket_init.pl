:- module(basket_init, []).

/** <module> LOD Basket: Initialization

Initializes the LOD Basket

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(apply)).

:- use_module(lle(basket/basket)).

:- initialization(init_basket).





init_basket:-
  absolute_file_name(lle('basket/seedpoints.pl'),  File, [access(read)]),
  thread_create(init_basket_from_file(File), _, []).

init_basket_from_file(File):-
  setup_call_cleanup(
    open(File, read, In),
    init_basket_from_stream(In),
    close(In)
  ).

init_basket_from_stream(In):-
  at_end_of_stream(In), !.
init_basket_from_stream(In):-
  read_term(In, Term, []),
  Term = uri(Uri),
  add_to_basket(Uri),
  init_basket_from_stream(In).

