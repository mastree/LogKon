/*:- include('inventory.pl'). */
:- include('battle.pl').

:- dynamic(lebarPeta/1).
:- dynamic(tinggiPeta/1).
:- dynamic(terrain/3).
:- dynamic(gymPos/2). /* (i,j) = (x,y) */
:- dynamic(mapStatus/1).
:- dynamic(player/2).

insideGym:-
    player(X,Y),
    gymPos(X,Y).

update_nearby :-
    player(X,Y),
    \+gymPos(X,Y),
    \+legendaryTokemon(_,_,_,_,_,X,Y),
    random(0,100,I),
    ((I < 5,
    startbattle);
    (I >= 5)), !.

update_nearby :-
    gymPos(X,Y),
    write('Anda sedang berada di gym!'), nl, !.

update_nearby :-
    legendaryTokemon(_,_,_,_,_,X,Y),
    startbattle.
    

init_map :-
    asserta(player(1,1)),
    random(15,31,X),
    random(15,31,Y),
    asserta(lebarPeta(X)),asserta(tinggiPeta(Y)),
    asserta(mapStatus(1)),
    generateTerrain,
    !.
 
/*isMapAset(G).*/
isTerrain('-').
isTerrain('X').
isWall('X').
   
printMap(X,Y) :-
    player(X,Y), \+ (gymPos(X,Y)), !, write('P').
printMap(X,Y) :-
    terrain(X,Y,Isi), write(Isi).
 
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

    asserta(legendaryTokemon('rinalmon', 'fire', 14000, 135180, 'Legendary',Lx,Ly)),
	asserta(legendaryTokemon('sangemon', 'water', 15000, 135171, 'Legendary',L2x,L2y)),

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
            ((\+ gymPos(I,J), \+ legendaryTokemon(_,_,_,_,_,I,J)),
            random(0,10,Pick),
            (Pick < 1,
            asserta(terrain(I,J,'X'));
            Pick >= 1,
            asserta(terrain(I,J,'-'))));
            (gymPos(I,J);(legendaryTokemon(_,_,_,_,_,I,J), asserta(terrain(I,J,'L'))))
        ))
    )),
    asserta(inventori('Pikachu', 'Listrik', 800, 3000, 'Normal')),
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
	player(X,Y),
    Xnew is X,
    Ynew is Y-1,
    terrain(Xnew,Ynew,Isi),
	isWall(Isi),
	write('Jangan ke atas lagi lah coy!'),nl,!.

w :-
	retract(player(X,Y)),
	Y > 1,
	NewY is Y-1,
	write([X,NewY]),nl,
	asserta(player(X,NewY)),
	write('Anda bergerak ke arah utara'),nl,
	update_nearby, !.

a :-
    player(X,Y),
    Xnew is X-1,
    Ynew is Y,
    terrain(Xnew,Ynew,Isi),
	isWall(Isi),
	write('Jangan ke kiri lagi lah coy!'),nl,!.	

a :-
	retract(player(X,Y)),
	X > 1,
	NewX is X-1,
	write([NewX,Y]),nl,
	asserta(player(NewX,Y)),
	write('Anda bergerak ke arah barat'),nl,
	update_nearby, !.

s :-
	player(X,Y),
    Xnew is X,
    Ynew is Y+1,
    terrain(Xnew,Ynew,Isi),
	isWall(Isi),
	write('Jangan ke bawah lagi lah coy!'),nl,!.

s :-
	retract(player(X,Y)),
	tinggiPeta(H),
	Y < H,
	NewY is Y+1,
	write([X,NewY]),nl,
	asserta(player(X,NewY)),
	write('Anda bergerak ke arah selatan'),nl,
	update_nearby, !.
d :-
	player(X,Y),
    Xnew is X+1,
    Ynew is Y,
    terrain(Xnew,Ynew,Isi),
	isWall(Isi),
	write('Jangan ke kanan lagi lah coy!'),nl,!.	

d :-
	retract(player(X,Y)),
	lebarPeta(W),
	X < W,
	NewX is X+1,
	write([NewX,Y]),nl,
	asserta(player(NewX,Y)),
	write('Anda bergerak ke arah timur'),nl,
	update_nearby, !.


/*
loads(_) :-
	gameMain(_),
	write('Kamu tidak bisa memulai game lainnya ketika ada game yang sudah dimulai.'), nl, !.
*/

loads(FileName):-
	\+file_exists(FileName),
	write('File tidak ditemukan!'), nl, !.
loads(FileName):-
	open(FileName, read, Str),
    read_file_lines(Str,Lines),
    close(Str),
    assertaList(Lines),
    write('Load game berhasil!'),nl, !.

/*
save(_):-
	\+gameMain(_),
	write('Command ini hanya bisa dipakai setelah game dimulai.'), nl,
	write('Gunakan command "start." untuk memulai game.'), nl, !.
*/

save(FileName):-
	tell(FileName),
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
		writeTerrain,
        writeLegendary,
        writeInventory,
	told, !.


writeTerrain:-
	\+terrain(_,_,_),
	!.
writeTerrain:-
	forall(terrain(X,Y,Na),(
		write('terrain('),write(X),write(','),
        write(Y),write(',\''),write(Na),write('\')'),write('.'), nl
	)), !.

writeLegendary:-
	forall(legendaryTokemon(A,B,C,D,E,F,G),(
		write(legendaryTokemon(A,B,C,D,E,F,G)), nl
	)), !.

writeInventory:-
    forall(inventori(A,B,C,D,E),(
		write(inventori(A,B,C,D,E)), nl
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
    \+legendaryTokemon(_,_,_,_,_,_,_),
    !.
retractLegendaryTokemon :-
    retract(legendaryTokemon(_,_,_,_,_,_,_)),
    retractLegendaryTokemon, !.
retractTerrain :-
    \+terrain(_,_,_),
    !.
retractTerrain :-
    retract(terrain(_,_,_)),
    retractTerrain, !.
quit :-
    retract(lebarPeta(_)),
    retract(tinggiPeta(_)),
    retractTerrain,
    retractLegendaryTokemon,
    retract(gymPos(_,_)),
    retract(mapStatus(_)),
    retract(player(_,_)), !.