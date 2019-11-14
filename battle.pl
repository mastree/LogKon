:- include('inventory.pl').

startbattle :- 
    write('A wild Tokemon appears!'),nl,
    write('Fight or Run?').

run :- 
    random(0,1,X),
    (X == 0,
    write('You failed to run!'),
    nl,
    write('Choose your Tokemon!'),
    nl,nl,battle);
    (X == 1,
    write('You succesfully escaped the Tokemon!')).

battle :-
    write('Available Tokemons: '),
    


