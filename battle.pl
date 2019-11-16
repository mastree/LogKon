:- include('inventory.pl').

:- dynamic(picked/1).
:- dynamic(enemy_pokemon/1).

:- discontiguous startbattle/0.
:- discontiguous run/0.
:- discontiguous battle/0.
:- discontiguous pick/1.
:- discontiguous damage/1.
:- discontiguous attack/0.

startbattle :- 
    write('A wild Tokemon appears!'),nl,
    write('Fight or Run?').

run :- 
    random(1,101,X),
    ((X < 71,
    write('You failed to run!'),
    nl,
    write('Choose your Tokemon!'),
    nl,nl,battle);
    (X >= 71,
    write('You succesfully escaped the Tokemon!'))),!.

battle :-
    write('Available Tokemons: '),nl,
    printStatus.

pick(X) :-
    \+picked(_),
    inventori(X,_,_,_,_),
    write(X),
    write(' I choose you!'),nl,
    nl,
    asserta(picked(X)),
    printStatus, !.

pick(X) :-
    \+inventori(X,_,_,_,_),
    write('You do not have that Tokemon!'), !.

pick(X) :-
    retract(picked(_)), pick(X), !.

damage(X) :-
    inventori(X,_,Y,_,_),!,
    write(Y).

attack :-
    nl,
    write('You dealt '),
    picked(X),
    damage(X),
    write('damage.'),nl.

