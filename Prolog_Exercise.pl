
replace(E1,[X|L1],E2,[Y|L2]):-
	(X=E1,append([E2],L1,[Y|L2]));
	(X=Y,replace(E1,L1,E2,L2)).

zip([],[],[]).
zip([A|As],[B|Bs],[A-B|ABs]):- zip(As,Bs,ABs).

sublist([],_).
sublist([X|Xs],[X|XSs]) :- sublist(Xs,XSs).
sublist([X|Xs],[_|XSs]) :- sublist([X|Xs],XSs).
	