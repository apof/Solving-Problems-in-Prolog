 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %Exw xrhsimopoihsei to kathgorhma element to opoio sthn teleytaia ekdosh tou eclipse pou exw sto pc mou (7.0#42)        %
 %mporei na parei ws deytero orisma lista metavlhtwn. Ara ston ypologisth mou exw ektelesei to programma paragwntas      %
 %swsta apotelesmata. Sta linux ths sxolhs pithanws logw diaforetikhs ekdoshs h element den dexetai ws deytero orisma    %
 %metavlhtes kai petaei error.                                                                                           %
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- set_flag(print_depth,1000).
:- lib(ic).
:- lib(branch_and_bound).
:- lib(ic_global).

solution_create(List,[],Index) :-  n_th(Index,List,Elem), Elem = 1.
solution_create(List,[Elem|Rest],Index) :- n_th(Index,List,Elem), Elem \= 1, solution_create(List,Rest,Elem).


indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  !,
  Index is Index1+1.

remove_dups([],[]).

remove_dups([H|T], [H|T1]) :-
    T \= [H|_],
    remove_dups(T, T1).

remove_dups([H,H|T], L) :-
    remove_dups([H|T], L).

init([],R,_) :-  R=<0.
init([List|Tail],Rows,Columns) :- Rows > 0,  length(List,Columns), List #::0..1, R is Rows -1, init(Tail,R,Columns).

srch([]).
srch([List|Tail]) :- search(List,0,first_fail,indomain_middle,complete,[]), srch(Tail).

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

n_th(1, [Node| _], Node).
n_th(N, [_| Nodes], Node) :-
N \= 1,
N1 is N - 1,
n_th(N1, Nodes, Node).

without_last_n(L, N, []) :-
    nonvar(L), nonvar(N),
    length(L, M),
    N > M.
without_last_n(L, N, R) :-
    without_last_n_(L, N, R).

without_last_n_(L, N, []) :-
    length(L, N).
without_last_n_([H|T], N, [H|T1]) :-
    without_last_n_(T, N, T1).
	
selectTowns(CostList,Number) :- N1 is Number - 1, costs(List), length(List,Total), reverse(List,New), 
N2 is Total - N1, without_last_n(New,N2,Neww), reverse(Neww,CostList).

create_cost_table(CostList,Number,Final) :-
create_up(Up,CostList,Number), length(Last,Number), append(Up,[Last],FinalUp),
transpose(FinalUp,FinalDown), put_inf(FinalUp), put_inf(FinalDown), result_cre(FinalUp,FinalDown,Final).

put_inf([]).
put_inf([List|Rest]) :- put_inff(List), put_inf(Rest).

put_inff([]).
put_inff([Elem|Rest]) :- Elem = 10000000, put_inff(Rest).
put_inff([Elem|Rest]) :- Elem \= 1000000, put_inff(Rest).


result_cre([],[],[]).
result_cre([List1|Rest1],[List2|Rest2],[Final|Tail]) :- append(List2,List1,Fin), 
remove_dups(Fin,Final),result_cre(Rest1,Rest2,Tail).

create_up([],[],_).
create_up([NewLine|Tail],[List|Rest],Number) :- 
length(List,N), Num is Number - N, length(Empty,Num), append(Empty,List,NewLine),create_up(Tail,Rest,Number).


tsp(N,Soll,Cost) :-

init(List,N,N),
constrain1(List),
transpose(List,TransList),
constrain1(TransList),

constrain2(List,R),
circuit(R),

selectTowns(CostList,N),
create_cost_table(CostList,N,Final),!,
cost_calc(List,Cost,0,Final),
append(List,[R],Res),
bb_min(srch(Res),Cost, bb_options{strategy:restart}),!, solution_create(R,Sol,1), append([1],Sol,Soll).
 

 
constrain1([]).
constrain1([List|Tail]) :-
sumlist(List,Sum),
Sum #= 1,
constrain1(Tail).

cost_calc([],Cost,Cost,[]).
cost_calc([],Cost,Cost,_).
cost_calc([List|Tail],Cost,New,[Vc|Tl]) :-
calc(List,LineCost,0,Vc),
N #= New + LineCost,
cost_calc(Tail,Cost,N,Tl).

calc([],Cost,Cost,[]).
calc([],Cost,Cost,_).
calc([Elem|Tail],Cost,New,[C2|Tl2]) :-
N #= New + Elem*C2,
calc(Tail,Cost,N,Tl2).

constrain2([],[]).
constrain2([List|Tail],[Index|Rest]) :- element(Index,List,1), constrain2(Tail,Rest).

 