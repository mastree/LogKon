:- include('inventory.pl').

:- dynamic(picked/1).
:- dynamic(enemy/4).
:- dynamic(sAttack/1).
:- dynamic(gameState/1).
:- dynamic(enemyDied/4).
:- dynamic(enemyMaxHP/1).


/*
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
*/
isGreater(fire,leaves).
isGreater(leaves,water).
isGreater(water,fire).

startbattle :-
    retract(gameState(_)),
    asserta(gameState(preBattle)),
    randTokemon(Nama,Tipe,Damage,Nyawa),
    asserta(enemy(Nama,Tipe,Damage,Nyawa)),
    asserta(enemyMaxHP(Nyawa)),
    asserta(sAttack(0)),
    printStatusEnemy,
    write('A wild Tokemon appears!'),nl,
    write('Fight or Run?').

startbattleLeg :-
    player(X,Y),
    retract(gameState(_)),
    asserta(gameState(preBattle)),
    legendaryTokemon(Nama,Tipe,Damage,Nyawa,_,_,X,Y),
    asserta(enemy(Nama,Tipe,Damage,Nyawa)),
    asserta(enemyMaxHP(Nyawa)),
    asserta(sAttack(0)),
    printStatusEnemy,
    write('A wild Legendary Tokemon appears!'),nl,
    write(Nama),write(' : "Hoho, mukatte kuru no ka?""'),nl,
    write('Fight or Run?').

run :-
    gameState(X), X \== preBattle, write('Illegal command.'), nl, !.

run :- 
    write('You : "Nigerundayo"'),nl,
    random(1,101,X),
    ((X < 71,
    write('You failed to run!'),
    nl, 
    write('Choose your Tokemon!'),
    nl,nl,fight);
    (X >= 71,
    write('You succesfully escaped the Tokemon!'),
    retract(enemy(_,_,_,_)),
    retract(gameState(_)),
    asserta(gameState(move)))), !.

fight :-
    gameState(X), X \== preBattle, write('Illegal command.'), nl, !.

fight :-
    /*write('You choosed to fight!'),nl,*/
    retract(gameState(_)), asserta(gameState(battle)),
    printMyStatus.

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
    write('Type: '),write(Tipe),nl,nl.

printMyStatus :-
    write('Your Tokemon: '), nl,nl,
    forall(inventori(Nama, Tipe, _, Nyawa, _, _),
    printMyToke(Nama, Tipe,_,Nyawa,_,_)).

pick(X) :-
    \+picked(_),
    inventori(X,_,_,_,_,_),
    write(X),
    write(' I choose you!'),nl,
    nl,
    printStatusEnemy,nl,
    asserta(picked(X)),
    printMyStatus,nl, !.

pick(X) :-
    \+inventori(X,_,_,_,_,_),
    write('You do not have that Tokemon!'), !.

pick(X) :-
    retract(picked(_)), pick(X), !.

damage(X) :-
    inventori(X,_,Y,_,_,_),!,
    write(Y).

attack :-
    write('You dealt '),
    picked(X),
    inventori(X,Tipe,Damage,_,_,_),
    enemy(Nama,TipeM,DamageM,CurrentNyawaM),
    retract(enemy(Nama,TipeM,DamageM,CurrentNyawaM)),
    ((isGreater(Tipe,TipeM),
    RealDamage is 1.5*Damage);
    (isGreater(TipeM,Tipe),
    RealDamage is 0.5*Damage);
    (\+(isGreater(Tipe,TipeM)),\+(isGreater(TipeM,Tipe)),
    RealDamage is Damage)),!,
    write(RealDamage),
    write(' damage '),
    write('to '),
    write(Nama),nl,
    NewCurrentNyawaM is CurrentNyawaM - RealDamage,
    ((NewCurrentNyawaM =< 0,
    write('You: "Yare yare daze"'),nl,
    write(Nama),
    write(' faints! Do you want to capture '),write(Nama),write('?'),
    write(' capture/0 to capture '),write(Nama),write(', otherwise move away.'),
    retract(sAttack(_)),
    retract(gameState(_)),
    ((legendaryTokemon(Nama,_,_,_,_,_,_,_),
    retract(legendaryTokemon(Nama,_,_,_,_,_,_,_)));
    (\+legendaryTokemon(Nama,_,_,_,_,_,_,_))),
    retract(picked(_)),
    asserta(enemyDied(Nama, TipeM, DamageM, CurrentNyawaM)),
    asserta(gameState(move)));
    (NewCurrentNyawaM > 0,
    asserta(enemy(Nama,TipeM,DamageM,NewCurrentNyawaM)),
    attackM)),!.


attackM :- 
    printStatusEnemy,
    nl,
    printMyStatus,
    nl,
    picked(X),
    write('==================================================================='),nl,
    enemy(NamaM,TipeM,DamageM,_),
    write(NamaM),
    write(' dealt '),
    retract(inventori(X,Tipe,Damage,CurrentNyawa,_,_)),
    ((isGreater(TipeM,Tipe),
    RealDamage is 1.5*DamageM);
    (isGreater(Tipe,TipeM),
    RealDamage is 0.5*DamageM);
    (\+isGreater(TipeM,Tipe),
    \+isGreater(Tipe,TipeM),
    RealDamage is DamageM)),!,
    write(RealDamage),
    write(' damage'),
    write(' to '),
    write(X),
    nl,
    nl,
    NewCurrentNyawa is CurrentNyawa - RealDamage,
    ((NewCurrentNyawa =< 0,
    write(X),
    write(' died.'), nl,
    currentInventoryLength(LengthNow),
    ((LengthNow =:= 0,
    retract(gameState(_)),
    retract(enemy(_,_,_,_)),
    retract(sAttack(_)),
    asserta(gameState(kalah)),lose);
    (LengthNow > 0,
    write('Your choice is died, pick another Tokemon!'),nl)));
    (NewCurrentNyawa > 0,
    asserta(inventori(X,Tipe,Damage,NewCurrentNyawa,_,_)),
    printStatusEnemy,
    nl,
    printMyStatus)),!.

specialAttack :-
    ((sAttack(1),
    write('Special attacks can only be used once per battle!'),nl);
    (sAttack(0),
    retract(sAttack(_)),
    asserta(sAttack(1)),
    picked(Nama),
    inventori(Nama,Tipe,Damage,_,_,_),
    retract(enemy(NamaM,TipeM,DamageM,CurrentNyawaM)),
    SpecDamage is 3*Damage, 
    write(Nama),
    write(' uses special attack!'),nl,
    write('It was super effective!'),nl,
    ((isGreater(Tipe,TipeM),
    RealDamage is 1.5*SpecDamage);
    (isGreater(TipeM,Tipe),
    RealDamage is 0.5*SpecDamage);
    (\+isGreater(TipeM,Tipe),
    \+isGreater(Tipe,TipeM),
    RealDamage is SpecDamage)),!,
    write('You dealt '),write(RealDamage),write(' damage to '),write(NamaM),nl,
    nl,
    NewCurrentNyawaM is CurrentNyawaM - RealDamage,
    ((NewCurrentNyawaM =< 0,
    write('You: "Yare yare daze"'),nl,
    write(NamaM),
    write(' faints! Do you want to capture '),write(NamaM),write('?'),
    write(' capture/0 to capture '),write(NamaM),write(', otherwise move away.'),
    retract(sAttack(_)),
    retract(gameState(_)),
    ((legendaryTokemon(NamaM,_,_,_,_,_,_,_),
    retract(legendaryTokemon(NamaM,_,_,_,_,_,_,_)));
    (\+legendaryTokemon(NamaM,_,_,_,_,_,_,_))),
    retract(picked(_)),
    asserta(enemyDied(NamaM, TipeM, DamageM, CurrentNyawaM)),
    asserta(gameState(move)));
    (NewCurrentNyawaM > 0,
    asserta(enemy(NamaM,TipeM,DamageM,NewCurrentNyawaM)),
    attackM)))),!.
  
capture :-
    enemyDied(NamaC,TipeC,DamageC,_),
    retract(enemyDied(NamaC,TipeC,DamageC,_)),
    enemyMaxHP(NyawaC),
    retract(enemyMaxHP(NyawaC)),
    write(NamaC),
    write(' is captured!'),
    ((legendaryTokemon(NamaC,_,_,_,_,_,_,_),
    addInventori(NamaC,TipeC,DamageC,NyawaC,legendary));
    (\+legendaryTokemon(NamaC,_,_,_,_,_,_,_),
    addInventori(NamaC,TipeC,DamageC,NyawaC,normal))), !.


status :-
    printMyStatus, !.



