:-lib(ic).
:-lib(ic_global).
:-set_flag(print_depth,1000).
:-lib(branch_and_bound).

vehicles([35, 40, 55, 15, 45, 25, 85, 55]).

clients([c(15,  77,  97), c(23, -28,  64), c(14,  77, -39),
         c(13,  32,  33), c(18,  32,   8), c(18, -42,  92),
         c(19,  -8,  -3), c(10,   7,  14), c(18,  82, -17),
         c(20, -48, -13), c(15,  53,  82), c(19,  39, -27),
         c(17, -48, -13), c(12,  53,  82), c(11,  39, -27),
         c(15, -48, -13), c(25,  53,  82), c(14, -39,   7),
         c(22,  17,   8), c(23, -38,  -7)]).

delMember(_, [], []) :- !.
delMember(X, [X|Xs], Y) :- !, delMember(X, Xs, Y).
delMember(X, [T|Xs], Y) :- !, delMember(X, Xs, Y2), append([T], Y2, Y).

delAllzeros([],[]).
delAllzeros([L|Tail],[Y|Result]) :- delMember(0,L,Y), delAllzeros(Tail,Result).

init([],_,NVe) :- NVe=<0.
init([List|Tail],NCl,NVe) :- NVe > 0,  length(List,NCl), List #::0..NCl, NVe2 is NVe -1, init(Tail,NCl,NVe2).

oc(List, NCl, NCl) :- !, occurrences(NCl, List, 1).
oc(List, I, NCl) :- occurrences(I, List, 1), Inew is I+1, oc(List, Inew, NCl).

colElem([X|_],1,X).
colElem([_|Tail],Col,Elem) :- C is Col - 1, colElem(Tail,C,Elem).

dest(New,Ncl,[]) :- New > Ncl.

dest(Ncl,[c(_,X,Y)|_],Clients,Ncl,[Res|Result]) :-
colElem(Clients,Ncl,c(_,XX,YY)), 
X1 is X - XX, Res1 is X1*X1, Y1 is Y - YY, Res2 is Y1*Y1, Res3 is Res1 + Res2,  sqrt(Res3,Re), Res is round(Re*1000),  New is Ncl + 1,
dest(New,Ncl,Result).

dest(N,[c(_,X,Y)|Tail],Clients,Ncl,[Res|Result]) :- N < Ncl,
colElem(Clients,N,c(_,XX,YY)), 
X1 is X - XX, Res1 is X1*X1, Y1 is Y - YY, Res2 is Y1*Y1, Res3 is Res1 + Res2,  sqrt(Res3,Re), Res is round(Re*1000), New is N + 1, 
dest(New,[c(_,X,Y)|Tail],Clients,Ncl,Result).


dest2(N,Ncl,[]) :- N > Ncl.
dest2(Ncl,Ncl,[X|Tail],Cl,[Res|Result]) :- dest(1,[X|Tail],Cl,Ncl,Res), New is Ncl + 1, dest2(New,Ncl,Result).
dest2(N,Ncl,[X|Tail],Cl,[Res|Result]) :- N < Ncl, dest(1,[X|Tail],Cl,Ncl,Res), New is N + 1, dest2(New,Ncl,Tail,Cl,Result).

q([],[],_).
q([c(Q,_,_)|Tail],[Q|Result],Fl) :- Fl = 1, q(Tail,Result,Fl).
q([c(Q,X,Y)|Tail],[Fl|Result],Fl) :- Fl = 0, New is Fl + 1, q([c(Q,X,Y)|Tail],Result,New).

constrain1([],_,_).
constrain1([X|Tail1],[V|Tail2],Q) :- con2(X,V,Q,0), constrain1(Tail1,Tail2,Q).

con2([],V,_,Sum) :- Sum #=< V.
con2([X|Tail],V,Q,Sum) :- XX #= X + 1, element(XX,Q,R), SumTmp #= Sum + R , con2(Tail,V,Q,SumTmp).

constrain3([]).
constrain3([List|Tail]) :- con3(List), constrain3(Tail).

con3([_|[]]).
con3([X1|[X2|Tail]]) :-
X2 #= X2 * (X1 #> 0),
con3([X2|Tail]).  

storage_add([],[],_).
storage_add([X|Tail],[X|Result],Fl) :- Fl = 1, storage_add(Tail,Result,Fl).
storage_add(List,[c(0,0,0)|Result],Fl) :- Fl = 0, New is Fl + 1, storage_add(List,Result,New).

int_convert([],[]).
int_convert([X|Tail],[Y|Result]) :- Y is integer(X), int_convert(Tail,Result).


cost([],_,Fin,_,Fin).

cost([V|VList],Distances,Cost,NCL,Final) :- 
storageClientCost(V,Distances,0,Tmp1), New1 #= Tmp1 + Cost,
vehicleCost(V,Distances,0,NCL,Tmp2),  New2 #= Tmp2 + New1,
cost(VList,Distances,New2,NCL,Final).

storageClientCost([X|_],Distances,Tmp,Cost) :-
Key #= X + 1 ,
element(Key,Distances,Co),
Cost #= Tmp + Co.

clientStorageCost(X,Distances,Tmp,NCL,Cost) :-
A is NCL + 1,
Key #= A*X + 0 + 1,
element(Key,Distances,C),
Cost #= Tmp + C.

vehicleCost([X|[]],Distances,Tmp,NCL,Cost) :- clientStorageCost(X,Distances,Tmp,NCL,Cost).
vehicleCost([X1,X2|Tail],Distances,Tmp,NCL,Cost) :-
A is NCL + 1,
Key #= A*X1 + X2 + 1,
element(Key,Distances,C),
NewCost #= Tmp + C,
vehicleCost([X2|Tail],Distances,NewCost,NCL,Cost).
  
hcvrp(NCl, NVe, Timeout, Result, Cost, Time) :-

statistics(times,[T1,_,_]),

vehicles(V),
clients(CL),
init(Solution,NCl,NVe),                               % initialize
flatten(Solution,SolFlat),							  % flatten solution list
oc(SolFlat,1,NCl),									  % kathe pelaths e3yphreteitai mia fora apo ena forthgo
q(CL,Q,0),											  % dhmiourgw lista me tis posothtes pou thelei kathe pelaths
constrain1(Solution,V,Q),							  % moirazw pelates se forthga analoga me th xwrhtikothta tou forthgou kai to poso pou zhta o pelaths
constrain3(Solution),								  % oi listes me tous pelates pou e3yphretei kathe forthgo exoun ta mhdenika sto telos

storage_add(CL,StorageList,0),							% prosthetw thn apothikh sth lista twn pelatwn wste na vrw tis oles tis apostaseis
New is NCl +1,
dest2(1,New,StorageList,StorageList,CostRes),			% vriskw oles tis apostaseis pelatwn apothikhs kai pelatwn meta3y tous
flatten(CostRes,CostResFlat),							% kanw flatten th lista me tis apostaseis
int_convert(CostResFlat,Int_List),
cost(Solution,Int_List,0,NCl,Cost),					% ypologizw to kostos

bb_min(search(SolFlat,0,input_order,indomain_middle,complete,[]),Cost, bb_options{strategy:restart,timeout:Timeout}),

delAllzeros(Solution,Result),

statistics(times,[T2,_,_]),
Time is T2-T1.
 
