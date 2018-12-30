compute_sum(0,S,S).
compute_sum(Num,S,Sum) :- SS is S + Num, NumNew is Num - 1, compute_sum(NumNew,SS,Sum).

sol_cre(_,N,Num,[]) :- Num - N =:= 0.

sol_cre(Max,N,Num,[El,N|Sol]) :-
	Num - N =:= 1,
	compute_sum(N,0,S),
	El is Max - S,
	NN is N + 1,
	sol_cre(Max,NN,Num,Sol).


sol_cre(Max,N,Num,[N|Sol]) :-
	H is Num - N,
	Num - N \= 1,
	NN is N + 1,
	sol_cre(Max,NN,Num,Sol).

biscuits(Num,B,L) :-
	sol_cre(Num,1,B,L).



