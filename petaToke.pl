:- include('util.pl').
:- include('11nov.pl').
 
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
    random(10,20,X),
    random(10,20,Y),
    asserta(lebarPeta(X)),asserta(tinggiPeta(Y)),
    asserta(mapStatus(1)),
    generateTerrain,!.
 
/*isMapAset(G).*/
isTerrain('-').
isTerrain('X').
   
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
    random(XMin, XMax, Gx),
    random(YMin, YMax, Gy),
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
            asserta(terrain(I,J,'-')));
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