:- set_flag(print_depth,1000).
:- lib(ic).
:- lib(branch_and_bound).

clique_size([],C,C).									% yplogizei posa stoixeia periexei h klika 
clique_size([X|Tail],Size,C) :- X=1, S is Size + 1, clique_size(Tail,S,C).
clique_size([X|Tail],Size,C) :- X=0, clique_size(Tail,Size,C).

clq([],[],_).										% dhmiourgei mia lista me tous komvous ths klikas
clq([X|Tail1],[Pos|Res],Pos) :- X=1, P is Pos + 1, clq(Tail1,Res,P).
clq([_|Tail1],Res,Pos) :- P is Pos + 1, clq(Tail1,Res,P).


maxclq(N,D,Clique_List,Size) :-
length(Clique,N),
create_graph(N,D,Graph),
Clique #:: 0..1,
constrain(Clique,Graph,1,2,Cost,N),
bb_min(search(Clique, 0, first_fail, indomain_middle, complete, []),Cost, bb_options{strategy:restart}),
clique_size(Clique,0,Size),
clq(Clique,Clique_List,1).



constrain([],_,_,_,C,C).								% dhmiourgei oles tis pithanes klikes
constrain([X|Tail],Graph,Count1,Count2,Cost,New) :- 
check_edge(X,Tail,Graph,Count1,Count2),
N #= New - X,
C1 is Count1 + 1,
C2 is Count2 + 1,
constrain(Tail,Graph,C1,C2,Cost,N).

check_edge(_,[],_,_,_).									% tsekarei ama kathe mia pithanh akmh yparxei sto grafo

check_edge(X,[Y|Tail],Graph,N1,N2) :-
not(member((N1-N2),Graph)),!,
X+Y #=< 1,
New is N2 + 1,
check_edge(X,Tail,Graph,N1,New).

check_edge(X,[_|Tail],Graph,N1,N2) :-
member((N1-N2),Graph),!,
New is N2 + 1,
check_edge(X,Tail,Graph,N1,New).



%%%%%%%%%%%%%%%% etoimo kathgorhma create graph %%%%%%%%%%%%%%%%%%%%%%%%%%%

create_graph(NNodes, Density, Graph) :-
   cr_gr(1, 2, NNodes, Density, [], Graph).

cr_gr(NNodes, _, NNodes, _, Graph, Graph).
cr_gr(N1, N2, NNodes, Density, SoFarGraph, Graph) :-
   N1 < NNodes,
   N2 > NNodes,
   NN1 is N1 + 1,
   NN2 is NN1 + 1,
   cr_gr(NN1, NN2, NNodes, Density, SoFarGraph, Graph).
cr_gr(N1, N2, NNodes, Density, SoFarGraph, Graph) :-
   N1 < NNodes,
   N2 =< NNodes,
   rand(1, 100, Rand),
   (Rand =< Density ->
      append(SoFarGraph, [N1 - N2], NewSoFarGraph) ;
      NewSoFarGraph = SoFarGraph),
   NN2 is N2 + 1,
   cr_gr(N1, NN2, NNodes, Density, NewSoFarGraph, Graph).

rand(N1, N2, R) :-
   random(R1),
   R is R1 mod (N2 - N1 + 1) + N1.
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


