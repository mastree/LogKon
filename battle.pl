:- include('inventory.pl').

:- dynamic(picked/1).
:- dynamic(enemy/4).



:- discontiguous startbattle/0.
:- discontiguous startbattleLeg/0.
:- discontiguous run/0.
:- discontiguous fight/0.
:- discontiguous pick/1.
:- discontiguous damage/1.
:- discontiguous attack/0.
:- discontiguous attackM/0.
:- discontiguous isGreater/2.
:- discontiguous printTokeEnemy/4.
:- discontiguous printMyStatus/0.
:- discontiguous printMyToke/6.
:- discontiguous printStatusEnemy/0.

isGreater(fire,leaves).
isGreater(leaves,water).
isGreater(water,fire).

startbattle :-
    retract(gameState(_)), asserta(gameState(preBattle)),
    randTokemon(Nama,Tipe,Damage,Nyawa),
    asserta(enemy(Nama,Tipe,Damage,Nyawa)),
    write('A wild Tokemon appears!'),nl,
    write('Fight or Run?').

startbattleLeg :- 
    legendaryTokemon(Nama,Tipe,Damage,Nyawa,_,_,_,_),
    asserta(enemy(Nama,Tipe,Damage,Nyawa)),
    write('A wild Tokemon appears!'),nl,
    write('Fight or Run?').

run :-
    gameState(X), X \= preBattle, write('Illegal command.'), nl, !.

run :- 
    random(1,101,X),
    ((X < 71,
    write('You failed to run!'),
    nl, retract(gameState(_)), asserta(gameState(battle)),
    write('Choose your Tokemon!'),
    nl,nl,battle);
    (X >= 71,
    write('You succesfully escaped the Tokemon!'))),!.

fight :-
    gameState(X), X \= preBattle, write('Illegal command.'), nl, !.

fight :-
    write('You choosed to fight!'),nl,
    retract(gameState(_)), asserta(gameState(battle)),
    printStatus.

printTokeEnemy(Nama,Tipe,_,Nyawa) :-
    write(Nama),nl,
    write('Health : '),write(Nyawa),nl,
    write('Type: '),write(Tipe),nl.

printStatusEnemy :-
    write('Your enemy: '), nl,nl,
    forall(enemy(Nama, Tipe, _, Nyawa),
    printTokeEnemy(Nama, Tipe,_,Nyawa)).

printMyToke(Nama,Tipe,_,Nyawa,_,_) :-
    write(Nama),nl,
    write('Health : '),write(Nyawa),nl,
    write('Type: '),write(Tipe),nl.

printMyStatus :-
    write('Your enemy: '), nl,nl,
    forall(inventori(Nama, Tipe, _, Nyawa, _, _),
    printMyToke(Nama, Tipe,_,Nyawa,_,_)).

pick(X) :-
    \+picked(_),
    inventori(X,_,_,_,_,_),
    write(X),
    write(' I choose you!'),nl,
    nl,
    printStatusEnemy,
    write('Your Tokemon: '),nl,
    asserta(picked(X)),
    printMyStatus, !.

pick(X) :-
    \+inventori(X,_,_,_,_,_),
    write('You do not have that Tokemon!'), !.

pick(X) :-
    retract(picked(_)), pick(X), !.

damage(X) :-
    inventori(X,_,Y,_,_,_),!,
    write(Y).

attack :-
    nl,
    write('You dealt '),
    picked(X),
    inventori(X,Tipe,Damage,_,_,_),
    retract(enemy(Nama,TipeM,DamageM,CurrentNyawaM)),
    ((isGreater(Tipe,TipeM),
    RealDamage is 1.5*Damage);
    (\+isGreater(Tipe,TipeM),
    RealDamage is Damage)),!,
    write(RealDamage),
    write(' damage '),
    write('to '),
    write(Nama),nl,
    NewCurrentNyawaM is CurrentNyawaM - RealDamage,
    NewCurrentNyawaM <= 0,retract(enemy(NamaMati,_,_,_)),
    write(NamaMati),
    write(' faints! Do you want to capture '),write(NamaMati),write('?'),
    write(' capture/0 to capture '),write(NamaMati),write(', otherwise move away.'));
    (NewCurrentNyawaM > 0,
    asserta(enemy(Nama,TipeM,DamageM,NewCurrentNyawaM)),
    attackM)).

/* belum fix */
attackM :- 
    printStatusEnemy,
    nl,
    printMyStatus,
    nl,
    enemy(NamaM,TipeM,DamageM,_),
    write(NamaM),
    write(' dealt '),
    retract(inventori(Nama,Tipe,Damage,CurrentNyawa)),
    ((isGreater(TipeM,Tipe),
    RealDamage is 1.5*DamageM);
    (\+isGreater(Tipe,TipeM),
    RealDamage is DamageM)),!,
    write(RealDamage),
    write(' damage'),
    write(' to '),
    write(Nama),
    nl,
    NewCurrentNyawa is CurrentNyawa - RealDamage,
    ((NewCurrentNyawa =< 0,
    retract(inventori(_,_,_,_,_,_)));
    (NewCurrentNyawa > 0,
    asserta(inventori(Nama,Tipe,Damage,NewCurrentNyawa)))).

    
