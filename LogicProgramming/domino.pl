dominos([(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),
(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),
(2,2),(2,3),(2,4),(2,5),(2,6),
(3,3),(3,4),(3,5),(3,6),
(4,4),(4,5),(4,6),
(5,5),(5,6),
(6,6)]).

frame([[3,1,2,6,6,1,2,2],
[3,4,1,5,3,0,3,6],
[5,6,6,1,2,4,5,0],
[5,6,4,1,3,3,0,0],
[6,1,0,6,3,2,4,0],
[4,1,5,2,4,3,5,5],
[4,1,0,2,4,5,2,0]]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%Sxolia%%%%%%%
% --->Arxika dhmiourgw mia lista h opoia gia kathe domino (A,B) ( topothethsh A,B h B,A ) periexei  listes ths morfhs [(X1,Y1,A),(X2,Y2,B)] opou X,Y oi sydetagmenes 
% mias pitanhs theshs tou A,B etsi opws exei prokypsei apo thn anazhthsh sto frame gia kathe domino. Sygekrimena X1,Y1 oi sydetagmenes tous A enw X2,Y2 tou B.
% H telikh loipon lista gia kathe domino periexei oles tis pithanes sydetagmenes stis opoies mporei na topotheththei, totso se ia grammh oso kai se mia sthlh.
% --->H lista ayth ta3inimeitai me quicksort kata ay3ousa seira mhkous twn ypolistwn wste oi mikres ypolistes, ta dominos dhladh pou exoun ligoterers enallaktikes
% theseis na vriskontai se arxikes theseis
% --->epeita me th member epilegetai ena stoixeio apo thn prwth ypolista, h delAll xrhsimopoiwntas thn delMember svhnei apo kathe epomenh ypolista stoixeia pou 
% erxontai se sygroush dhladh akyrwnontai ws enallaktikes vasei ths epiloghs pou kaname me th Member, h nea lista ta3inomeotai kai kaleitai h synarthsh anadromika
% gia thn epomenh ypolista. Otan h member klhthei na epile3ei apo kenh lista apotygxanei kai opisthrdomei se allh lysh enw ama telika ftasoume sto telos kai h 
% member kataferei apo thn teleytaia ypolista na epile3ei ena stoixeio tote exoume ftasei sthn telikh mas lysh.
% !!! Ws pros thn ektypwsh den akoloutheitai to format ths ekfwnhshs all mia aplousterh ektypwsh tou domino kai tou slot sydetagmenwn opou topotheteitai telika,
% pou anaparista omws to teliko zhtoumeno tou provlhmatos 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


put_dominos :- find_positions_sorted(R), find_final_list(R,Res), len_of_frame(Rows,Cols),
write("Solution for a frame with: "), write(Rows), write(" Rows and "), write(Cols), writeln(" Columns"),
print_solution(Res),

print_solution([]).
print_solution([[(X1,Y1,A),(X2,Y2,B)]|Tail]) :- write("Domino: ("),write(A),write(","),write(B),write(") in slot: "),write("[("),write(X1),write(","),write(Y1),
write("),("),write(X2),write(","),write(Y2),writeln(")]"),
print_solution(Tail).

len_of_frame(Rows,Cols) :- frame(F), len(F,Rows), len_cols(F,Cols).
   

find_final_list([],[]).
find_final_list([Sublist|Tail],[Elem|Result]) :- 
member(Elem,Sublist),  
delAll(Elem,Tail,UpdateList),
quicksort(UpdateList,NewSorted),
find_final_list(NewSorted,Result).  

delAll(_,[],[]).
delAll(Elem,[Y|Tail],[Final|Res]) :- 
delMember(Elem,Y,Final),
delAll(Elem,Tail,Res).

delMember(_, [], []) :- !.
delMember([A,B], [[A,_]|Xs], Y) :- !, delMember([A,B], Xs, Y).
delMember([A,B], [[_,A]|Xs], Y) :- !, delMember([A,B], Xs, Y).
delMember([A,B], [[B,_]|Xs], Y) :- !, delMember([A,B], Xs, Y).
delMember([A,B], [[_,B]|Xs], Y) :- !, delMember([A,B], Xs, Y).
delMember(X, [T|Xs], [T|Y]) :- !, delMember(X, Xs, Y).


find_positions_sorted(Sorted) :-  dominos(L), frame(F), search(L,F,FinalList), !, quicksort(FinalList,Sorted).

search([],_,[]).
search([(X,Y)|Tail],Frame,[Res|FinalList]) :-
search_row(Frame,X,Y,1,1,2,Res1), search_col(Frame,X,Y,1,2,Res2), append(Res1,Res2,Res),search(Tail,Frame,FinalList).

search_row([],_,_,_,_,_,[]).
search_row([[Head1|[Head2|Tail]]|List],X,Y,Row,Col,Col2,Result) 
:- pr([Head1|[Head2|Tail]],X,Y,Row,Col,Col2,Res1), R is Row + 1, search_row(List,X,Y,R,1,2,Res2), append(Res1,Res2,Result).


pr([_|[]],_,_,_,_,_,[]).

pr([Head1|[Head2|Tail]],X,Y,Row,Col,Col2,[[(Row,Col,X),(Row,Col2,Y)]|Sublist]) 
:- X=Head1, Y=Head2, C is Col + 1, C2 is Col2 + 1,  pr([Head2|Tail],X,Y,Row,C,C2,Sublist).

pr([Head1|[Head2|Tail]],X,Y,Row,Col,Col2,[[(Row,Col,Y),(Row,Col2,X)]|Sublist]) 
:- Y=Head1, X=Head2, C is Col + 1, C2 is Col2 + 1,  pr([Head2|Tail],X,Y,Row,C,C2,Sublist).

pr([_|[Head2|Tail]],X,Y,Row,Col,Col2,Sublist) :- C is Col + 1, C2 is Col2 + 1,  pr([Head2|Tail],X,Y,Row,C,C2,Sublist).


search_col([_|[]],_,_,_,_,[]).
search_col([[Head1|Tail1]|[[Head2|Tail2]|List]],X,Y,Row,Row2,Result) 
:- pr([Head1|Tail1],[Head2|Tail2],X,Y,Row,1,Row2,Res1), R is Row + 1, R2 is Row2 + 1, search_col([[Head2|Tail2]|List],X,Y,R,R2,Res2), append(Res1,Res2,Result).


pr([],[],_,_,_,_,_,[]).

pr([X1|L1],[X2|L2],X,Y,Row,Col,Row2,[[(Row,Col,X),(Row2,Col,Y)]|Sublist]) 
:- X = X1, Y = X2, C is Col + 1, pr(L1,L2,X,Y,Row,C,Row2,Sublist).

pr([X1|L1],[X2|L2],X,Y,Row,Col,Row2,[[(Row,Col,Y),(Row2,Col,X)]|Sublist]) 
:- Y = X1, X = X2, C is Col + 1, pr(L1,L2,X,Y,Row,C,Row2,Sublist).

pr([_|L1],[_|L2],X,Y,Row,Col,Row2,Sublist) :- C is Col + 1, pr(L1,L2,X,Y,Row,C,Row2,Sublist).


quicksort([], []).
quicksort([HEAD | TAIL], SORTED) :-
                                    len(HEAD,Len),partition(Len, TAIL, LEFT, RIGHT),
                                    quicksort(LEFT, SORTEDL),
                                    quicksort(RIGHT, SORTEDR),
                                    append(SORTEDL, [HEAD | SORTEDR], SORTED).

partition(_, [], [], []).
partition(PIVOT, [HEAD | TAIL], [HEAD | LEFT], RIGHT) :- len(HEAD,Len),	Len @=< PIVOT, partition(PIVOT, TAIL, LEFT, RIGHT).
partition(PIVOT, [HEAD | TAIL], LEFT, [HEAD | RIGHT]) :- len(HEAD,Len), Len @> PIVOT, partition(PIVOT, TAIL, LEFT, RIGHT).

len([], LenResult):-
    LenResult is 0.

len([_|Y], LenResult):-
    len(Y, L),
    LenResult is L + 1.

len_cols([Sublist|_], LenResult) :-
	len(Sublist,LenResult).