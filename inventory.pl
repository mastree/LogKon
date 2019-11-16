:- dynamic(inventori/5).
:- dynamic(legendaryTokemon/7).
:- dynamic(maxInventori/1).

setNamaTipe(yuckmon, water).
setNamaTipe(platypus, water).
setNamaTipe(bragamon, fire).
setNamaTipe(apimon, fire).
setNamaTipe(daunmon, leaves).
setNamaTipe(greenmon, leaves).

currentInventoryLength(Length) :-
	findall(A, inventori(A, _, _, _, _), ListLength),
	length(ListLength, Length), !.

addInventori(A,B,C,D,E) :-
	currentInventoryLength(Panjang),
	maxInventori(Max),
	Panjang + 1 > Max, !, fail.

addInventori(Nama, Tipe, Damage, Nyawa, Rarity) :-
	asserta(inventori(Nama, Tipe, Damage, Nyawa, Rarity)),!.

delInventori(Nama) :-
	retract(inventori(Nama, _, _, _, _)),!.

printStatus :-
	write('Kisama no Tokemon :'),nl,nl,
	forall(inventori(A,B,C,D,E), printTokemon(A,B,C,D,E)),
	write('================================='), nl,
	nl, write('Lejenderi Tokemon :'), nl,nl,
	forall(legendaryTokemon(F,G,H,I,J), printTokemon(F,G,H,I,J)).

printTokemon(Nama, Tipe, _, Nyawa, _) :-
	write(Nama), nl,
	write('Health : '), write(Nyawa), nl,
	write('Tipe : '), write(Tipe), nl, nl.

randTokemon(Nama, Tipe, Damage, Nyawa) :-
	findall([A,B], setNamaTipe(A,B), ListToke),
	length(ListToke, Panjang),
	random(0, Panjang, Ith),
	nth0(Ith, ListToke, Dapet),
	nth0(0, Dapet, N),
	nth0(1, Dapet, T),
	Nama = N,
	Tipe = T,
	random(0, 100, Tier),
	(Tier<80, 
	random(1000, 5000, Hp);
	Tier>79,
	Tier<93, 
	random(5000, 15000, Hp);
	Tier>92, 
	random(15000, 45000, Hp)),
	Nyawa is Hp,
	(Tier<80, 
	random(200, 1000, Atk);
	Tier>79,
	Tier<93, 
	random(1000, 3000, Atk);
	Tier>92, 
	random(3000, 9000, Atk)),
	Damage is Atk,!.
