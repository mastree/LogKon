:- include('util.pl').
/*:- include('move.pl').*/
 
/*:- dynamic(deadzone/1).*/
:- dynamic(lebarPeta/1).
:- dynamic(tinggiPeta/1).
/*:- dynamic(tick/1).*/
:- dynamic(terrain/3).
:- dynamic(gymPos/2). /* (i,j) = (x,y) */
:- dynamic(mapStatus/1).
:- dynamic(player/2).
 
init_map :-
    asserta(player(1,1)),
    random(10,21,X),
    random(10,21,Y),
    asserta(lebarPeta(X)),asserta(tinggiPeta(Y)),
    asserta(mapStatus(1)),
    generateTerrain,!.
 
/*isMapAset(G).*/
isTerrain('-').
isTerrain('X').
isWall('X').
   
printMap(X,Y) :-
    player(X,Y), !, write('P').
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
    random(XMin, Xp, Gx),
    random(YMin, Yp, Gy),
    asserta(gymPos(Gx,Gy)),
    asserta(terrain(I,J,'G')),
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
            ((I \== Gx;
            J \== Gy),
            /*
            findall(Ter,isTerrain(Ter),ListTerrain),
            length(ListTerrain, Panjang),
            random(0,Panjang,NoTerrain),
            ambil(ListTerrain, NoTerrain, Terrain),
            asserta(terrain(I,J,Terrain)))*/
            random(0,10,Pick),
            (Pick < 1,
            asserta(terrain(I,J,'X'));
            Pick >= 1,
            asserta(terrain(I,J,'-'))));
            (I == Gx;
            J == Gy)
        ))
    )),
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
	/* check ketemu pokemon atau kagak */!.

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
	/* check ketemu pokemon atau kagak */!.

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
	/* check ketemu pokemon atau kagak */!.
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
	/* check ketemu pokemon atau kagak */!.
