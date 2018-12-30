zip([],[],[]).
zip([],_,[]).
zip(_,[],[]).
zip([Head1|Tail1],[Head2|Tail2],[(Head1,Head2)|Tail]) :- zip(Tail1,Tail2,Tail).

calc_cost([],[]).
calc_cost([(X,Y)|Tail],[CC|C]) :- X >= Y, CC is X - Y, calc_cost(Tail,C).
calc_cost([(X,Y)|Tail],[CC|C]) :- Y >= X, CC is Y - X, calc_cost(Tail,C).

find_perms(L1,L2,Max) :- length(L1,N1), length(L2,N2), N1 > N2, 
permutation(Perm1,L1),zip(Perm1,L2,P),calc_cost(P,Cost),max_list(Cost,Max).
find_perms(L1,L2,Max) :- length(L1,N1), length(L2,N2), N2 >= N1,
permutation(Perm2,L2),zip(L1,Perm2,P),calc_cost(P,Cost),max_list(Cost,Max).

ugliness(L1,L2,First) :- findall(L,find_perms(L1,L2,L),Sol),sort(Sol,[First|_]).