/* Implementasi Move */

w :-
	player(_,Y),
	Y =:= 1,
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
	player(X,_),
	X =:= 1,
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
	player(_,Y),
	tinggiPeta(H),
	Y =:= H,
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
	player(X,_),
	lebarPeta(W),
	X =:= W,
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
