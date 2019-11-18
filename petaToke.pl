:- include('util.pl').
:- include('battle.pl').

:- dynamic(lebarPeta/1).
:- dynamic(tinggiPeta/1).
:- dynamic(terrain/3).
:- dynamic(gymPos/2). /* (i,j) = (x,y) */
:- dynamic(mapStatus/1).
:- dynamic(player/2).
:- dynamic(healstatus/1).
/* gameState menerima string dengan pilihan:
    move : Lagi explore.
    battle : Lagi battle.
    preBattle : Lagi milih antara fight atau run.
    gym : Lagi di gym.
    menang : Udah menang.
    kalah : Udah kalah.
*/
/*:- dynamic(enemyMaxHP/1).*/

insideGym:-
    player(X,Y),
    gymPos(X,Y).

heal :-
    insideGym,
    \+healstatus(1),
    retract(healstatus(0)),
    forall(inventori(Name,_,_,_,_,_), healTokemon(Name)),
    asserta(healstatus(1)), !.

heal :-
    healstatus(1),
    write('You\'ve used your only chance to heal. No more for you!'),nl,nl,!.
heal :-
    \+insideGym,
    write('You\'re currently not in Gym Center. You can only heal in Gym Center!'), nl,nl, !.

healTokemon(Nama) :-
    retract(inventori(Nama,Tipe,Damage,_,MaxHP,Rarity)),
    NewNyawa is MaxHP,
    asserta(inventori(Nama,Tipe,Damage,NewNyawa,MaxHP,Rarity)), !.

update_nearby :-
    \+legendaryTokemon(_,_,_,_,_,_,_,_),
    retract(gameState(_)),
    asserta(gameState(menang)),
    win, !.

update_nearby :-
    gameState(kalah),
    lose, !.

update_nearby :-
    player(X,Y),
    \+gymPos(X,Y),
    \+legendaryTokemon(_,_,_,_,_,_,X,Y),
    random(0,100,I),
    ((I < 38,
    startbattle);
    (I >= 38)), !.

update_nearby :-
    player(X,Y),
    gymPos(X,Y),
    write('Anda sedang berada di gym!'), nl, !.

update_nearby :-
    player(X,Y),
    legendaryTokemon(_,_,_,_,_,_,X,Y),
    startbattleLeg.
    

init_map :-
    asserta(player(1,1)),
    random(15,31,X),
    random(15,31,Y),
    asserta(lebarPeta(X)),asserta(tinggiPeta(Y)),
    asserta(mapStatus(1)),
    generateTerrain,
    asserta(gameState(move)),
    asserta(healstatus(0)),
    !.
 
/*isMapAset(G).*/
isTerrain('-').
isTerrain('X').
isWall('X').
   
printMap(X,Y) :-
    player(X,Y), \+ (gymPos(X,Y)), !, write('P').
printMap(X,Y) :-
    (terrain(X,Y,Isi), Isi == 'L', write('-'));(terrain(X,Y,Isi), Isi \== 'L', write(Isi)).
 
generateTerrain:-
    lebarPeta(L),
    tinggiPeta(T),
    XMin is 1,
    XMax is L,
    YMin is 1,
    YMax is T,
    Xp is (L+1),
    Yp is (T+1),

    Gx is div(L,2),
    Gy is div(T,2),
    asserta(gymPos(Gx,Gy)),    
    asserta(terrain(Gx,Gy,'G')),

    Gxp is Gx+1,

    random(XMin, Gx, Lx),
    random(YMin, Yp, Ly),

    random(Gxp, Xp, L2x),
    random(YMin, Yp, L2y),

/*
    ((L2xtm /== Lx; L2ytm/== Ly),
    L2x is L2xtm, L2y is L2ytm;
    (L2xtm == Lx; L2ytm== Ly),
    L2x is L2xtm, L2y is L2ytm+1),
*/

    asserta(legendaryTokemon('rinalmon', 'fire', 2000, 15180, 15180, 'Legendary',Lx,Ly)),
	asserta(legendaryTokemon('sangemon', 'water', 2600, 15171, 15171, 'Legendary',L2x,L2y)),

    forall(between(0,Yp,J), (
        asserta(terrain(0,J,'X')),
        asserta(terrain(Xp,J,'X'))
    )),
    forall(between(0,Xp,I), (
        asserta(terrain(I,0,'X')),
        asserta(terrain(I,Yp,'X'))
    )),
    forall(between(YMin,YMax,J), (
        forall(between(XMin,XMax,I), (
            ((\+ gymPos(I,J), \+ legendaryTokemon(_,_,_,_,_,_,I,J)),
            random(0,10,Pick),
            (Pick < 1,
            asserta(terrain(I,J,'X'));
            Pick >= 1,
            asserta(terrain(I,J,'-'))));
            (gymPos(I,J);(legendaryTokemon(_,_,_,_,_,_,I,J), asserta(terrain(I,J,'L'))))
        ))
    )),
    /* addInventori('Intmander', fire, 800, 3000, 'Normal'), udh milih di picking phase*/
	asserta(maxInventori(6)),
    !.
 
printMapAll:-
    lebarPeta(L),
    tinggiPeta(T),
    XMin is 0,
    XMax is (L+1),
    YMin is 0,
    YMax is (T+1),
    mapStatus(1),
    forall(between(YMin,YMax,J), (
        forall(between(XMin,XMax,I), (
            printMap(I,J)
        )),
        nl
    )),
    !.

/* Implementasi Move */
w :-
    \+gameState(move),
    write('You\'re currently not able to move! Face your destiny!'),nl,!.
w :-
    gameState(X), X == preBattle,
    write('You\'re encountering a tokemon. You can\'t move! Fight or run?'), nl, !.

w :-
	player(X,Y),
    Xnew is X,
    Ynew is Y-1,
    terrain(Xnew,Ynew,Isi),
	isWall(Isi),
	write('There\'s an obstacle there! You have to find your way around!'),nl,!.

w :-
    ((enemyMaxHP(_),
    retract(enemyMaxHP(_)));
    (\+enemyMaxHP(_))),
	retract(player(X,Y)),
	Y > 1,
	NewY is Y-1,
	write([X,NewY]),nl,
	asserta(player(X,NewY)),
	write('You moved towards north.'),nl,
	update_nearby, !.
a :-
    \+gameState(move),
    write('You\'re currently not able to move! Face your destiny!'),nl,!.
a :-
    gameState(X), X == preBattle,
    write('You\'re encountering a tokemon. You can\'t move! Fight or run?'), nl, !.

a :-
    player(X,Y),
    Xnew is X-1,
    Ynew is Y,
    terrain(Xnew,Ynew,Isi),
	isWall(Isi),
	write('There\'s an obstacle there! You have to find your way around!'),nl,!.	

a :-
    ((enemyMaxHP(_),
    retract(enemyMaxHP(_)));
    (\+enemyMaxHP(_))),
	retract(player(X,Y)),
	X > 1,
	NewX is X-1,
	write([NewX,Y]),nl,
	asserta(player(NewX,Y)),
	write('You moved towards west.'),nl,
	update_nearby, !.
s :-
    \+gameState(move),
    write('You\'re currently not able to move! Face your destiny!'),nl,!.
s :-
    gameState(X), X == preBattle,
    write('You\'re encountering a tokemon. You can\'t move! Fight or run?'), nl, !.

s :-
	player(X,Y),
    Xnew is X,
    Ynew is Y+1,
    terrain(Xnew,Ynew,Isi),
	isWall(Isi),
	write('There\'s an obstacle there! You have to find your way around!'),nl,!.

s :-
    ((enemyMaxHP(_),
    retract(enemyMaxHP(_)));
    (\+enemyMaxHP(_))),
	retract(player(X,Y)),
	tinggiPeta(H),
	Y < H,
	NewY is Y+1,
	write([X,NewY]),nl,
	asserta(player(X,NewY)),
	write('You moved towards south.'),nl,
	update_nearby, !.

d :-
    \+gameState(move),
    write('You\'re currently not able to move! Face your destiny!'),nl,!.

d :-
    gameState(X), X == preBattle,
    write('You\'re encountering a tokemon. You can\'t move! Fight or run?'), nl, !.

d :-
	player(X,Y),
    Xnew is X+1,
    Ynew is Y,
    terrain(Xnew,Ynew,Isi),
	isWall(Isi),
	write('There\'s an obstacle there! You have to find your way around!'),nl,!.	

d :-
    ((enemyMaxHP(_),
    retract(enemyMaxHP(_)));
    (\+enemyMaxHP(_))),
	retract(player(X,Y)),
	lebarPeta(W),
	X < W,
	NewX is X+1,
	write([NewX,Y]),nl,
	asserta(player(NewX,Y)),
	write('You moved towards east.'),nl,
	update_nearby, !.


/* win or lose */
win :-
    write('You did it! You\'ve defeated all of the Legendary Tokemons!'),nl,
    write('As you can see from afar, Doctor Zomboss is headed towards you!'),nl,
    write('.'),nl,
    write('.'),nl,
    write('.'),nl,
    write('.'),nl,
    write('.'),nl,
    write('Now, Doctor Zomboss is right in front of you'),nl, nl,
    write('\'Congratulations for your outstanding achievement!\', says Doctor Zomboss'),nl,nl,
    write('As you\'ve heard that, a surge of happiness is running in your heart along with a big smile on your face'),nl,
    write('Because you know,'),nl,
    write('you'),nl,
    write('have'),nl,
    write('become'),nl,
    write('THE VERY BEST LIKE NO ONE EVER WAS'),nl,nl,
    quit, !.

lose :-
    write('You\'ve lost all your trusted companions!'),nl,
    write('There\'s no other way for you to go on in this journey.'),nl,
    write('This is the end of your journey.'),nl,
    write('As you sit down on the ground and face your own failure,'),nl,
    write('The thought suddenly reached your mind.'),nl,
    write('Instead of becoming the very best,'),nl,
    write('You have become the VERY WORST.'),nl,nl,
    quit, !.
/*
loads(_) :-
	gameMain(_),
	write('Kamu tidak bisa memulai game lainnya ketika ada game yang sudah dimulai.'), nl, !.
*/

loads(FileName):-
	\+file_exists(FileName),
	write('File not found!'), nl, !.
loads(FileName):-
	open(FileName, read, Str),
    read_file_lines(Str,Lines),
    close(Str),
    assertaList(Lines),
    write('Load game berhasil!'),nl, !.

save(FileName):-
    gameState('move'),
    tell(FileName),
        gameState(A),
        write(gameState(A)),write('.'),nl,
		player(X,Y),
		write(player(X,Y)),write('.'),nl,
        gymPos(Gx,Gy),
        write(gymPos(Gx,Gy)),write('.'),nl,
        lebarPeta(Lebar),
        write(lebarPeta(Lebar)),write('.'),nl,
        tinggiPeta(Tinggi),
        write(tinggiPeta(Tinggi)),write('.'),nl,
        mapStatus(Stat),
        write(mapStatus(Stat)),write('.'),nl,
        maxInventori(Minv),
        write(maxInventori(Minv)),write('.'),nl,
		writeTerrain,
        writeLegendary,
        writeInventory,
        healstatus(Eel),
        write(healstatus(Eel)),write('.'),nl,
	told, !.

save(_):-
    \+gameState('move'),
    write('You can\'t save yet!'),nl,nl,!.

writeTerrain:-
	\+terrain(_,_,_),
	!.
writeTerrain:-
	forall(terrain(X,Y,Na),(
		write('terrain('),write(X),write(','),
        write(Y),write(',\''),write(Na),write('\')'),write('.'), nl
	)), !.

writeLegendary:-
	forall(legendaryTokemon(A,B,C,D,E,F,G,H),(
		write(legendaryTokemon(A,B,C,D,E,F,G,H)), nl
	)), !.

writeInventory:-
    forall(inventori(A,B,C,D,E,F),(
		write(inventori(A,B,C,D,E,F)), nl
	)), !.

/* Read dari file eksternal */
readData(S,[]) :-
	at_end_of_stream(S), !.

readData(S,[X1|Tail]) :-
	get_char(S,X1),
	readData(S,Tail).

baca_file(NamaFile,Isi) :-
	open(NamaFile,read,S),
	repeat,
	readData(S,Isi),
	close(S),!.

/* Membaca file menjadi list of lines */
read_file_lines(Stream,[]) :-
    at_end_of_stream(Stream).

read_file_lines(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_file_lines(Stream,L).
/*-----------------------------*/
/* Write ke file eksternal */
writeData(_,[]) :- !.
writeData(S,[X1|Tail]) :-
	write(S,X1),
	writeData(S,Tail).

write_list(NamaFile,L) :-
	open(NamaFile,write,S),
	repeat,
	writeData(S,L),
	close(S).
/*-----------------------------*/
retractLegendaryTokemon :-
    \+legendaryTokemon(_,_,_,_,_,_,_,_),
    !.
retractLegendaryTokemon :-
    retract(legendaryTokemon(_,_,_,_,_,_,_,_)),
    retractLegendaryTokemon, !.
retractTerrain :-
    \+terrain(_,_,_),
    !.
retractTerrain :-
    retract(terrain(_,_,_)),
    retractTerrain, !.

retractInventory :-
    \+inventori(_,_,_,_,_,_), !.

retractInventory :-
    retract(inventori(_,_,_,_,_,_)),
    retractInventory, !.
quit :-
    retract(lebarPeta(_)),
    retract(tinggiPeta(_)),
    retractTerrain,
    retractLegendaryTokemon,
    retract(gymPos(_,_)),
    retract(mapStatus(_)),
    retract(player(_,_)),
    retract(gameState(_)),
    retractInventory,
    retract(healstatus(_)), !.