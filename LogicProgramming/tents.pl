%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sxolia panw sto programma:
% To programma exei elegxthei kai paragei swsta apotelesmata gia oles tis eisodous, mono pou gia tis megalyteres xreiazetai arketo xrono
% Endeiktika gia ta input ths ekfwnhshs ( dokimh ston ypologisth mou ):
% Input1: 
% Yes (0.00s cpu, solution 1, maybe more)
% findall Yes (0.02s cpu)
% Input2: 
% Yes (13.70s cpu, solution 1, maybe more)
% findall Yes (53.73s cpu)
% Input3:
% Yes (132.20s cpu, solution 1, maybe more)
% Afto ofeileiletai kyriws sthn ylopoihsh tou periorismou gia toulaxiston mia geitonikh tenta gyrw apo kathe dentro
% gia ton opoio  exw xrhsimipoihsei polla kathgorhmata gia na kataferw na kalypsw oles tis periptwseis topothethshs tou dentrou panw ston agro
% Genikes parathrhseis:
% xrhsimopoieitai montelopoihsh listwn apo listes gia thn anaparastash tou plaisiou
% vrisketai to veltisto kostos me bbmin kai epeita oles oi lyseis me search pou antistoixoun se afto xrhsimopoiwntas to min cost ws periorismo 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-lib(ic).
:-lib(ic_global).
:-set_flag(print_depth,1000).
:-lib(branch_and_bound).

init([],R,_) :-  R=<0.
init([List|Tail],Rows,Columns) :- Rows > 0,  length(List,Columns), List #::0..1, R is Rows -1, init(Tail,R,Columns).
 
cost_sum([],Sum,Sum).
cost_sum([List|Tail],Sum,Num) :- sumlist(List,N), New #= Num + N, cost_sum(Tail,Sum,New).

srch([]).
srch([List|Tail]) :- search(List,0,input_order,indomain_middle,complete,[]), srch(Tail).

convert1([],_,[]).
convert1([List|Tail],Row,Result) :- convert2(List,Row,1,Res1), R is Row + 1, append(Res1,Res2,Result), convert1(Tail,R,Res2).
  
convert2([],_,_,[]).
convert2([X|Tail],Row,Col,Result) :- X=0, C is Col + 1, convert2(Tail,Row,C,Result).
convert2([X|Tail],Row,Col,[(Row-Col)|Result]) :- X=1, C is Col + 1, convert2(Tail,Row,C,Result).
 
tents(RowTents, ColumnTents, Trees, FinalResult) :-
length(RowTents,Rows), length(ColumnTents,Columns),
init(Result,Rows,Columns),
cost_sum(Result,Cost,0),
constrain(Result,Trees,Rows,Columns,RowTents, ColumnTents),
bb_min(srch(Result),Cost, bb_options{strategy:restart}),


length(RowTents,Rows2),
length(ColumnTents,Columns2),
init(Result2,Rows2,Columns2),
cost_sum(Result2,MinCost,0),
constrain2(Result2,Trees,Rows2,Columns2,RowTents, ColumnTents,MinCost,Cost),
srch(Result2),
convert1(Result2,1,FinalResult).

constrain2(Result,Trees,Rows,Columns,RowTents, ColumnTents,MinCost,Cost) :-

Cost#=MinCost,

constrain1(Result,Trees,1,1,Rows,Columns),	% periorismos na mhn yparxoun tentes opou yparxoun dentra
constrain2(RowTents,Result),			% periorismos gia ton megisto ana grammh arithmo tentwn
constrain3(ColumnTents,Result,1),		% periorismos gia to megisto ana sthlh arithmo tentwn
constrain4(Result),				% periorismps gia mh geitonikes tentes
constrain5(Trees,Result,Rows,Columns).		% periorismos gia toulaxiston mia tenta gyrw apo kathe dentro


constrain(Result,Trees,Rows,Columns,RowTents, ColumnTents) :-

constrain1(Result,Trees,1,1,Rows,Columns),	% periorismos na mhn yparxoun tentes opou yparxoun dentra
constrain2(RowTents,Result),			% periorismos gia ton megisto ana grammh arithmo tentwn
constrain3(ColumnTents,Result,1),		% periorismos gia to megisto ana sthlh arithmo tentwn
constrain4(Result),				% periorismps gia mh geitonikes tentes
constrain5(Trees,Result,Rows,Columns).		% periorismos gia toulaxiston mia tenta gyrw apo kathe dentro



%%%%%%%%%%%%%%

constrain1([[]|[]],_,_,_,_,_).

constrain1([[Head|Tail]|List],Trees,Row,Col,Rows,Columns) :-
C is Col + 1,
check1(Head,Trees,Row,Col),
constrain1([Tail|List],Trees,Row,C,Rows,Columns).

constrain1([[]|List],Trees,Row,_,Rows,Columns) :-
R is Row + 1,
constrain1(List,Trees,R,1,Rows,Columns).

check1(Head,Trees,Row,Col) :- member((Row-Col),Trees), Head #= 0.
check1(_,Trees,Row,Col) :- not(member((Row-Col),Trees)).

%%%%%%%%%%%%%%%

constrain2([],[]).
constrain2([Elem|RowTentsTail],[_|Result]) :- Elem = -1, constrain2(RowTentsTail,Result).
constrain2([Elem|RowTentsTail],[List|Result]) :- Elem \= -1, sumlist(List,N), N #=< Elem, constrain2(RowTentsTail,Result).

%%%%%%%%%%%%%%%

constrain3([],_,_).
constrain3([Elem|ColumnTentsTail],Result,Col) :- Elem = -1, CNew is Col + 1, constrain3(ColumnTentsTail,Result,CNew).
constrain3([Elem|ColumnTentsTail],Result,Col) :- Elem \= -1, colSum(Result,Col,0,Sum), Sum #=< Elem, CNew is Col + 1, constrain3(ColumnTentsTail,Result,CNew).

colSum([],_,Sum,Sum).
colSum([List|Tail],Col,Sum,FinSum) :- colElem(List,Col,Elem), S #= Sum + Elem, colSum(Tail,Col,S,FinSum).

colElem([X|_],1,X).
colElem([_|Tail],Col,Elem) :- C is Col - 1, colElem(Tail,C,Elem).

%%%%%%%%%%%%%%%%

constrain4([_|[]]).
constrain4([[Head1|Tail1]|[[Head2|Tail2]|List]]) :- checkTents([Head1|Tail1],[Head2|Tail2],1,0), constrain4([[Head2|Tail2]|List]).

checkTents([_|[]],_,_,_).

checkTents([Elem|[Next|Tail]],Row2,1,Sum) :- Col = 1,
S1 #= Sum + Elem, S2 #= S1 + Next, colElem(Row2,Col,ElemDown), Cnext is Col + 1, colElem(Row2,Cnext,ElemDownNext),
S3 #= S2 + ElemDown, S4 #= S3 + ElemDownNext, S4 #=< 1, checkTents([Next|Tail],Row2,Cnext,0).

checkTents([Elem|[Next|Tail]],Row2,Col,Sum) :-
S1 #= Sum + Elem, S2 #= S1 + Next, Cprev is Col - 1, Cnext is Col + 1, colElem(Row2,Col,ElemDown), colElem(Row2,Cprev,ElemDownPrev), colElem(Row2,Cnext,ElemDownNext),
S3 #= S2 + ElemDown, S4 #= S3 + ElemDownNext, S4 #=<1,
S5 #= Elem + ElemDownPrev, S5 #=<1,
checkTents([Next|Tail],Row2,Cnext,0).

%%%%%%%%%%%%%%%%

findRow([Row|_],1,Row).
findRow([_|Tail],R,Elem) :- RNew is R - 1, findRow(Tail,RNew,Elem).

constrain5([],_,_,_).
constrain5([(X-Y)|Tail],Result,MaxR,MaxC) :- checkTrees(X,Y,Result,MaxR,MaxC), constrain5(Tail,Result,MaxR,MaxC).


checkTrees(1,1,Result,_,_) :-
findRow(Result,1,R1),findRow(Result,2,R2),
colElem(R1,1,Elem), colElem(R1,2,ElemNext), colElem(R2,1,ElemDown),colElem(R2,2,ElemDownNext),
Sum #= Elem + ElemNext + ElemDown + ElemDownNext, Sum #>= 1.

checkTrees(1,MaxCol,Result,_,MaxCol) :-
findRow(Result,1,R1),findRow(Result,2,R2),
colElem(R1,MaxCol,Elem), ColPrev is MaxCol -1, colElem(R1,ColPrev,ElemPrev), colElem(R2,MaxCol,ElemDown),colElem(R2,ColPrev,ElemDownPrev),
Sum #= Elem + ElemPrev + ElemDown + ElemDownPrev, Sum #>= 1.

checkTrees(1,Col,Result,_,_) :- 
findRow(Result,1,R1),findRow(Result,2,R2),
colElem(R1,Col,Elem), CNext is Col + 1, CPrev is Col - 1, colElem(R1,CNext,ElemNext), colElem(R1,CPrev,ElemPrev),
colElem(R2,Col,ElemDown),colElem(R2,CNext,ElemDownNext),colElem(R2,CPrev,ElemDownPrev),
Sum #= Elem + ElemNext + ElemPrev + ElemDown + ElemDownNext + ElemDownPrev, Sum #>= 1.

checkTrees(MaxRow,1,Result,MaxRow,_) :-
PrevRow is MaxRow -1, findRow(Result,MaxRow,R1),findRow(Result,PrevRow,R2),
colElem(R1,1,Elem), colElem(R1,2,ElemNext), colElem(R2,1,ElemUp),colElem(R2,2,ElemUpNext),
Sum #= Elem + ElemNext + ElemUp + ElemUpNext, Sum #>= 1.

checkTrees(MaxRow,MaxCol,Result,MaxRow,MaxCol) :-
PrevRow is MaxRow -1, findRow(Result,MaxRow,R1),findRow(Result,PrevRow,R2),
colElem(R1,MaxCol,Elem), ColPrev is MaxCol -1, colElem(R1,ColPrev,ElemPrev), colElem(R2,MaxCol,ElemUp),colElem(R2,ColPrev,ElemUpPrev),
Sum #= Elem + ElemPrev + ElemUp + ElemUpPrev, Sum #>= 1.

checkTrees(MaxRow,Col,Result,MaxRow,_) :-
PrevRow is MaxRow -1, findRow(Result,MaxRow,R1),findRow(Result,PrevRow,R2),
colElem(R1,Col,Elem), CNext is Col + 1, CPrev is Col - 1, colElem(R1,CNext,ElemNext), colElem(R1,CPrev,ElemPrev),
colElem(R2,Col,ElemUp),colElem(R2,CNext,ElemUpNext),colElem(R2,CPrev,ElemUpPrev),
Sum #= Elem + ElemNext + ElemPrev + ElemUp + ElemUpNext + ElemUpPrev, Sum #>= 1.

checkTrees(Row,1,Result,_,_) :-
PrevRow is Row - 1, NextRow is Row + 1, findRow(Result,PrevRow,R1),findRow(Result,Row,R2),findRow(Result,NextRow,R3),
colElem(R2,1,Elem), colElem(R2,2,ElemNext), colElem(R1,1,ElemUp), colElem(R1,2,ElemUpNext), colElem(R3,1,ElemDown), colElem(R3,2,ElemDownNext), 
Sum #= Elem + ElemNext + ElemUp + ElemUpNext + ElemDown + ElemDownNext,  Sum #>= 1.

checkTrees(Row,MaxCol,Result,_,MaxCol) :- 
PrevRow is Row - 1, NextRow is Row + 1, findRow(Result,PrevRow,R1),findRow(Result,Row,R2), findRow(Result,NextRow,R3),
ColPrev is MaxCol - 1,
colElem(R2,MaxCol,Elem), colElem(R2,ColPrev,ElemPrev), colElem(R1,MaxCol,ElemUp), colElem(R1,ColPrev,ElemUpPrev), colElem(R3,MaxCol,ElemDown), colElem(R3,ColPrev,ElemDownPrev), 
Sum #= Elem + ElemPrev + ElemUp + ElemUpPrev + ElemDown + ElemDownPrev,  Sum #>= 1.


checkTrees(Row,Col,Result,_,_) :- Row\=1, Col\=1,
PrevRow is Row - 1, NextRow is Row + 1, findRow(Result,PrevRow,R1),findRow(Result,Row,R2), findRow(Result,NextRow,R3),
ColPrev is Col - 1, ColNext is Col + 1, 
colElem(R2,Col,Elem), colElem(R2,ColPrev,ElemPrev), colElem(R2,ColNext,ElemNext),
colElem(R3,Col,ElemDown), colElem(R3,ColPrev,ElemDownPrev), colElem(R3,ColNext,ElemDownNext),
colElem(R1,Col,ElemUp), colElem(R1,ColPrev,ElemUpPrev), colElem(R1,ColNext,ElemUpNext), 
Sum #= Elem + ElemPrev + ElemNext +  ElemUp + ElemUpPrev + ElemUpNext + ElemDown + ElemDownPrev + ElemDownNext,  Sum #>= 1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




