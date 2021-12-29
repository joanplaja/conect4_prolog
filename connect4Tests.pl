% Import connect4 predicates
:- [connect4].

test_checkPlayerCollection_humanWins :-
    analizeConjunt([ [x,x,x,-,-,-,-],[x,x,x,x,-,-,-] ],huma),
    analizeConjunt([ [x,x,x,x,o,o,o],[x,x,x,-,-,-,-] ],huma).

test_checkPlayerCollection_cpuWins :-
    analizeConjunt([ [x,x,x,o,o,o,o],[x,x,x,x,o,o,o] ],cpu).

test_checkPlayerCollection_cpuLooses :-
    not(analizeConjunt([ [x,x,x,x,o,o,o],[x,x,x,-,o,x,o] ],cpu)).

