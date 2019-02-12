%%%%%%%%%%%%%%%%%%%%%% to programma trexei mono sthn teleytaia ekdosh tou eclipse logw %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% idiaterothtas ths element pou exw anaferei kai sthn prohgoumenh ergasia %%%%%%%%%%%%%%%%%%%

:- set_flag(print_depth,1000).
:- lib(ic).
:- lib(branch_and_bound).
:- lib(ic_global).

print_Vars([]).
print_Vars([[P,_,A,L,R,W]|Tail]) :-
write("P: "), writeln(P),
write("A: "), writeln(A),
write("L: "), writeln(L),
write("R: "), writeln(R),
write("W: "), writeln(W),
writeln("--------------------------"), print_Vars(Tail).

init([],R,_) :-  R=<0.
init([[P,PP,A,L,R,W]|Tail],Rows,Columns) :- 
Rows > 0,
length(P,Columns), length(PP,Columns),  length(A,Columns), length(L,Columns), length(R,Columns), length(W,Columns),
P #::0..1, PP #::0..1,  A #::0..1, L #::0..1, R #::0..1, W #::0..1,  
Ro is Rows -1, init(Tail,Ro,Columns).

rehearsal(Sequence2, Cost, Timeout) :-

durations(Durations),
musicians(Slots),
length(Slots,MusicNumber),
length(Durations,Pieces),
length(Sequence,Pieces),
length(Sequence2,Pieces),
Sequence #:: 1..Pieces,
Sequence2 #:: 1..Pieces,
all_different(Sequence),
all_different(Sequence2),
constrain0(Sequence,Sequence2,1),
init(Vars,MusicNumber,Pieces),
constrain1(Vars,Sequence,Slots),
constrain2(Vars),
constrain3(Vars),
constrain4(Vars),
constrain5(Vars,Sequence,Sequence2),
cost_calc(Vars,Durations,Cost,0),
bb_min(search(Sequence,0,first_fail,indomain_middle,complete,[]),Cost, bb_options{strategy:restart,timeout:Timeout}),
print_Vars(Vars), writeln(Sequence2).


constrain0([],_,_).
constrain0([Seq1|Tail1],Seq2,N) :- element(Seq1,Seq2,N), NN is N + 1, constrain0(Tail1,Seq2,NN).  

all_different([]).
all_different([Head|Tail]) :- all_diff(Head,Tail), all_different(Tail).
all_diff(_,[]).
all_diff(Head,[X|Tail]) :- Head #\= X, all_diff(Head,Tail).

n_th(1, [Node| _], Node).
n_th(N, [_| Nodes], Node) :-
N \= 1,
N1 is N - 1,
n_th(N1, Nodes, Node).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% P[k][Sequence[j]] = Slots[k][j] %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
constrain1([],_,[]).
constrain1([[P,PP,_,_,_,_]|Vars],Sequence,[S|Slots]) :- con1(P,PP,Sequence,S,1), constrain1(Vars,Sequence,Slots).
con1(_,[],_,[],_).
con1(P,[Elem|Tail],Sequence,[S|Slots],N) :- n_th(N,Sequence,Dj),
element(Dj,P,Elem), Elem #= S,
NN is N + 1, con1(P,Tail,Sequence,Slots,NN). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% A[k][1] = P[k][1]%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% A[k][i] = 1 iff A[k][i-1] = 1 or P[k][i] = 1 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
constrain2([]).
constrain2([[P,_,A,_,_,_]|Vars]) :- con2(P,A,1), constrain2(Vars).
con2([],[_|[]],_).
con2([P|Tail1],[A|Tail2],1) :- A #= P , con2(Tail1,[A|Tail2],2).
con2([P|Tail1],[A1,A2|Tail2],2) :- A2 #= (A1 or P) , con2(Tail1,[A2|Tail2],2).

constrain3([]).
constrain3([[P,_,_,L,_,_]|Vars]) :- reverse(P,Rev), con3(Rev,LL,1), reverse(LL,L), constrain3(Vars).
con3([],[_|[]],_).
con3([P|Tail1],[LL|Tail2],1) :- LL #= P , con3(Tail1,[LL|Tail2],2).
con3([P|Tail1],[LL1,LL2|Tail2],2) :- LL2 #= (LL1 or P) , con3(Tail1,[LL2|Tail2],2).



%%%%%%%%%%%% R[k][i] = A[k][i]*L[k][i] %%%%%%%%%
constrain4([]).
constrain4([[_,_,A,L,R,_]|Vars]) :- con4(A,L,R), constrain4(Vars).
con4([],[],[]).
con4([A|Tail1],[L|Tail2],[R|Tail3]) :- R #= A*L, con4(Tail1,Tail2,Tail3).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% if (P = 0 and R = 1) then W[k][Sequence[j]] = 1 else = 0    %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
constrain5([],_,_).
constrain5([[P,_,_,_,R,W]|Vars],Sequence,Sequence2) :- con5(P,R,W,Sequence,1,Sequence2), constrain5(Vars,Sequence,Sequence2).
con5([],[],_,_,_,_).
con5([P|Tail1],[R|Tail2],W,Sequence,N,Sequence2) :- 
n_th(N,Sequence2,Dj),
element(Dj,W,Elem), Elem #= ((P #= 0) and (R #= 1)),
NN is N + 1, con5(Tail1,Tail2,W,Sequence,NN,Sequence2).
 
%%%%%%%%%%% calculate cost as sum: W[i][j]*duration[j] for every i,j %%%%%%%%%%%%%%%%%%%%%%%
cost_calc([],_,C,C).
cost_calc([[_,_,_,_,_,W]|Vars],Durations,Cost,C) :-
cc(W,Durations,Co,0),
New #= C + Co,
cost_calc(Vars,Durations,Cost,New).

cc([],[],C,C).
cc([W|Tail],[D|Duration],Cost,C) :-
New #= C + W*D,
cc(Tail,Duration,Cost,New).