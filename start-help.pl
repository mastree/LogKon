:- include('battle.pl').
:- include('move.pl').
:- include('petaToke.pl').

:- dynamic(gameState/1).

:- discontiguous init_game/0.
:- discontiguous help/0.
:- discontiguous start/0.
:- discontiguous map/0.

init_game :-
	asserta(gameState(main)),
	write(' ____  _____  _  _  ____  __  __  _____  _  _ '),nl,
	write('(_  _)(  _  )( )/ )( ___)(  \\/  )(  _  )( \\( ) '), nl,
	write('  )(   )(_)(  )  (  )__)  )    (  )(_)(  )  ( '), nl,
	write(' (__) (_____)(_)\\_)(____)(_/\\/\\_)(_____)(_)\\_)'),nl,
	write(' ____  ____  _____     _     __    _____  ___'),nl,
	write('(  _ \\(  _ \\(  _  )   ( )   (  )  (  _  )/ __) '),nl,
	write(' )___/ )   / )(_)(    /_\\/   )(__  )(_)(( (_-.'),nl,
	write('(__)  (_)\\_)(_____)  (__/\\  (____)(_____)\\___/ '),nl,
	nl,
	write('Gotta catch them all!'),nl,
	nl,
	write('Hello there! Welcome to the world of Tokemon! My name is Aril! People call me the Tokemon Professor! This world is inhabited by creatures called Tokemon! There are hundreds of Tokemon loose in Labtek 5! You can catch them all to get stronger, but what I am really interested in are the 2 legendary Tokemons, Icanmon dan Sangemon. If you can defeat or capture all those Tokemons I will not kill you.'),nl,
	nl,
	write('Available commands: '), nl,
	write(' start. --start the game!'), nl,
	write(' help. --show available commands'), nl,
	write(' quit. --quit the game'), nl,
	write(' n. s. e. w. --move'), nl,
	write(' map. --look at the map'), nl,
	write(' heal --cure Tokemon in inventory if in gym center'), nl,
	write(' status. --show your status'), nl,
	write(' save(Filename). --save your game'), nl,
	write(' load(Filename). --load previously saved game'), nl,
	nl,
	write('Legends: '),nl,
	write(' -X = Pagar'),nl,
	write(' -P = Player'),nl,
	write(' -G = Gym'),nl, !.
help :-
	write('Available commands: '), nl,
	write(' start. --start the game!'), nl,
	write(' help. --show available commands'), nl,
	write(' quit. --quit the game'), nl,
	write(' w. a. s. d. --move'), nl,
	write(' map. --look at the map'), nl,
	write(' heal --cure Tokemon in inventory if in gym center'), nl,
	write(' status. --show your status'), nl,
	write(' save(Filename). --save your game'), nl,
	write(' load(Filename). --load previously saved game'), nl,
	nl,
	write('Legends: '),nl,
	write(' -X = Pagar'),nl,
	write(' -P = Player'),nl,
	write(' -G = Gym'),nl.

start :-
	gameState(X), X == play,
	write('You\'re already playing! Can\'t start new game'), nl, !.

start :-
	gameState(X), X == main, retract(gameState(_)), asserta(gameState(pick)),
	write('Game will be started!'), nl,
	write('Are you ready to catch them all?'), nl,
	write('===================================='),nl,
	write('You\'re a youngster who want to explore the world and taming monsters called \'Tokemons\'.'), nl,
	write('The world is a big place, and you need all the preparation you can get.'), nl,
	write('Doctor Zomboss has given you a chance to select one of three tokemon to get started.'), nl,
	write('===================================='), nl,
	write('Rilamon'), nl,
	write('Type : Fire'), nl,
	write('HP : 100'), nl,
	write('Level : 1'), nl, nl,
	write('Harlimon'), nl,
	write('Type : Water'), nl,
	write('HP : 100'), nl,
	write('Level : 1'), nl, nl,
	write('Rinamon'), nl,
	write('Type : Leaves'), nl,
	write('HP : 100'), nl,
	write('Level : 1'), nl, nl,
	write('===================================='), nl,
	write('Choose wisely : '), !.

rilamon :-
	gameState(X), X \= pick, write('Illegal command.'), nl, !.

rilamon :-
	gameState(X), X == pick, write('You have picked Rilamon as your starter!'), nl,
	asserta(inventori('rilamon', fire, 10, 100, 100, 'common')),
	write('Now go outside there and catch some legendary tokemons.'), nl,
	write('===================================='), nl,
	retract(gameState(_)), assert(gameState(play)), init_map, map, !.

map :-
	gameState(X), X == main,
	write('You haven\'t even started the game. Please type \'start.\' first.'), nl, !.	

map :-
	gameState(X), X == play,
	printMapAll, !.

:- init_game, !.
