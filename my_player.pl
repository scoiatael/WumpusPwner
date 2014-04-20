/* -*- mode: Prolog; comment-column: 48 -*- */

/****************************************************************************
 *
 * Copyright (c) 2012 Witold Paluszynski
 *
 * I grant everyone the right to copy this program in whole or part, and
 * to use it for any purpose, provided the source is properly acknowledged.
 *
 * Udzielam kazdemu prawa do kopiowania tego programu w calosci lub czesci,
 * i wykorzystania go w dowolnym celu, pod warunkiem zacytowania zrodla.
 *
 ****************************************************************************/


/*
  This program implements a simple agent strategy for the wumpus world.
  The agent ignores all dangers of the wumpus world.
  The strategy is to go forward, and turn left if bumped into a wall.
*/

% standard action generating rules
% this is our agent's algorithm, the rules will be tried in order
act(Action, Knowledge) :- ( 
		check_for_imperative(Action, Knowledge)
	;	pick_up_gold(Action, Knowledge)
	;	exit_if_possible(Action, Knowledge)
	;	( 
			(	safe_spot
		;		wumpus_nearby
		;		quite_a_view
			),
			(	explore(Action, Knowledge)
		;		kill_wumpus(Action, Knowledge)
			)
		)
							).

check_for_imperative(A, K) :- 
	its_time_to(X), 
	X(A,K).

pick_up_gold(A, K) :-
	glitter,
	Action = grab,
	assert(have_gold),	
	assert(its_time_to(run)),
	pack_knowledge(K).

exit_if_possible(A, K) :-
	have_gold,
	am_at((0,0)),
	Action = exit,
	K = [].

safe_spot :-
	\+ stench,
	\+ breeze,
	am_at(X),
	neighbours(X,Y),
	add_safe_spots(Y).

wumpus_nearby :-
	assert(fuck).

quite_a_view :-
	assert(photo).

explore(Action, Knowledge) :- 
	is_safe_spot(X),
	begin_journey_to(X,A),
	pack_knowledge(Knowledge).

kill_wumpus(A,K) :-
	assert(not_gonna_happen).

	

