:- set_flag(print_depth,1000).
:- lib(ic).
:- lib(branch_and_bound).
:- lib(ic_global).

%%%%%%%%%%%%%%%% etoimo kathgorhma create graph %%%%%%%%%%%%%%%%%%%%%%%%%%%

res_create([],[],_).										
res_create([X|Tail1],[Pos|Res],Pos) :- X=1, P is Pos + 1, res_create(Tail1,Res,P).
res_create([_|Tail1],Res,Pos) :- P is Pos + 1, res_create(Tail1,Res,P).

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

grpart(N,D,L1,L2,Cost) :-
create_graph(N,D,Graph),
length(List1,N),
length(List2,N),
length(Graph,Edges),
length(EdgeList,Edges),
EdgeList #:: 0..1,
List1 #:: 0..1,
List2 #:: 0..1,
sumlist(List1,Sum1),
sumlist(List2,Sum2),
(Sum1 #= Sum2) or (Sum1 #= Sum2 + 1) or (Sum2 #= Sum1 + 1),
Sum1 + Sum2 #= N,
dif_constrain(List1,List2),
constrain(List1,List2,EdgeList,Graph),
sumlist(EdgeList,Cost),
append(List1,List2,List),
bb_min(search(List, 0, first_fail, indomain_middle, complete, []),Cost, bb_options{strategy:restart}),
res_create(List1,L1,1), res_create(List2,L2,1).

n_th(1, [Node| _], Node).
n_th(N, [_| Nodes], Node) :-
N \= 1,
N1 is N - 1,
n_th(N1, Nodes, Node).

dif_constrain([],[]).
dif_constrain([El1|List1],[El2|List2]) :- El1 #\= El2, dif_constrain(List1,List2).

constrain(_,_,[],[]).
constrain(List1,List2,[Edge|EdgeList],[N1-N2|Graph]) :-
n_th(N1,List1,Node1), n_th(N2,List1,Node2),
n_th(N1,List2,Node11), n_th(N2,List2,Node22),
Edge #= (Node1*Node22) + (Node11*Node2),
constrain(List1,List2,EdgeList,Graph).






