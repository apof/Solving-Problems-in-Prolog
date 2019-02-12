:- set_flag(print_depth,1000).
:- lib(ic).
:- lib(branch_and_bound).
:- lib(ic_global).

indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  !,
  Index is Index1+1.

init([],R,_) :-  R=<0.
init([List|Tail],Rows,Columns) :- Rows > 0,  length(List,Columns), List #::0..1, R is Rows -1, init(Tail,R,Columns).

srch([]).
srch([List|Tail]) :- search(List,0,first_fail,indomain_middle,complete,[]), srch(Tail).

create_list([],[]).
create_list([List|Tail],[R|Res]) :- indexOf(List,1,Index), R is Index + 1, create_list(Tail,Res).

choose_params(N1,M1,N,M) :- N1 = 0, M1 = 0, fixedcosts(Fc),varcosts(Vc), length(Fc,N), length(Vc,M).
choose_params(N1,M1,N,M) :- N1 = 0, M1 \= 0, fixedcosts(Fc),varcosts(Vc), length(Fc,N), length(Vc,M).
choose_params(N1,M1,N,M) :- N1 \= 0, M1 = 0, fixedcosts(Fc),varcosts(Vc), length(Fc,N), length(Vc,M).
choose_params(N1,M1,N1,M1) :- N1 \= 0, M1 \= 0. 

warehouses(N1,M1,Location,Res,Cost) :-
choose_params(N1,M1,N,M),
init(Supply,M,N),
length(Location,N),
Location #:: 0..1,
constrain1(Supply),
constrain2(Supply,Location),
fixedcosts(Fc),
fixed_cost_calc(Fc,Location,C,0),
varcosts(Vc),
cost_calc(Supply,1,1,Cost,C,Vc),
append([Location],Supply,List),
bb_min(srch(List),Cost, bb_options{strategy:restart}),
create_list(Supply,Res).


constrain1([]).
constrain1([List|Tail]) :-
sumlist(List,Sum),
Sum #= 1,
constrain1(Tail).

constrain2([],_).
constrain2([List|Rest],Loc) :-
con(List,Loc),
constrain2(Rest,Loc).

con([],[]).
con([Supp|Tail1],[Loc|Tail2]) :-
(Loc #= 0) => (Supp #= 0),
con(Tail1,Tail2).

cost_calc([],_,_,Cost,Cost,[]).
cost_calc([],_,_,Cost,Cost,_).
cost_calc([List|Tail],Col,Row,Cost,New,[Vc|Tl]) :-
calc(List,1,Row,LineCost,0,Vc),
R is Row + 1,
N #= New + LineCost,
cost_calc(Tail,Col,R,Cost,N,Tl).


calc([],_,_,Cost,Cost,[]).
calc([],_,_,Cost,Cost,_).
calc([Elem|Tail],Col,Row,Cost,New,[C2|Tl2]) :-
N #= New + Elem*C2,
C is Col + 1,
calc(Tail,C,Row,Cost,N,Tl2).

fixed_cost_calc([],[],Cost,Cost).
fixed_cost_calc(_,[],Cost,Cost).
fixed_cost_calc([Cost|Tail1],[Location|Tail2],C,New) :-
N #= New + Location*Cost,
fixed_cost_calc(Tail1,Tail2,C,N).







