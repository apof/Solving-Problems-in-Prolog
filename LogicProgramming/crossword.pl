crossword(Solution) :- 
match_words_to_spaces(R,Spaces), quicksort(R,S),find_final_list(S,Solution1),dimension(Size),printSolFormated(1,1,Size,Solution1),
findfinalSol(Solution1,Spaces,Solution2),!,findSol2(Solution2,Solution).

printSolFormated(Row,Col,Max,Sol) :- Row=Max, Col=Max,  letterGet(Row,Col,Sol,Letter), Letter\=black, writeln(Letter).
printSolFormated(Row,Col,Max,Sol) :- Row=Max, Col=Max,  letterGet(Row,Col,Sol,Letter), Letter=black, writeln('#').
printSolFormated(Row,Col,Max,Sol) :- Row=Max, Col\=Max, letterGet(Row,Col,Sol,Letter), Letter\=black, write(Letter), write(" "), C is Col + 1, printSolFormated(Row,C,Max,Sol).
printSolFormated(Row,Col,Max,Sol) :- Row=Max, Col\=Max, letterGet(Row,Col,Sol,Letter), Letter=black, write('#'), write(" "), C is Col + 1, printSolFormated(Row,C,Max,Sol).
printSolFormated(Row,Col,Max,Sol) :- Row\=Max, Col\=Max,  letterGet(Row,Col,Sol,Letter), Letter\=black, write(Letter), write(" "), C is Col + 1, printSolFormated(Row,C,Max,Sol).
printSolFormated(Row,Col,Max,Sol) :- Row\=Max, Col\=Max,  letterGet(Row,Col,Sol,Letter), Letter=black, write('#'),write(" "), C is Col + 1, printSolFormated(Row,C,Max,Sol).
printSolFormated(Row,Col,Max,Sol) :- Row\=Max, Col=Max,  letterGet(Row,Col,Sol,Letter), Letter\=black, write(Letter),write("\n"), R is Row + 1, printSolFormated(R,1,Max,Sol).
printSolFormated(Row,Col,Max,Sol) :- Row\=Max, Col=Max,  letterGet(Row,Col,Sol,Letter), Letter=black, write('#'),write("\n"), R is Row + 1, printSolFormated(R,1,Max,Sol).

letterGet(_,_,[],black).
letterGet(R,C,[(Word,Coords)|_],Letter) :- member((R,C),Coords), indexOf(Coords,(R,C),N), match(Word,N,Letter).
letterGet(R,C,[(_,Coords)|Tail],Letter) :- not member((R,C),Coords), letterGet(R,C,Tail,Letter).

findSol2([],[]).
findSol2([Head|Rest],[R|Result]) :- ff2(Head,Res1), name(R,Res1), findSol2(Rest,Result).
ff2([],[]).
ff2([Head|Tail],[Res|Result]) :- name(Head,[Res]), ff2(Tail,Result). 

findfinalSol(_,[],[]).
findfinalSol(Solution,[Coord|Tail],[Word|Result]) :- member((Word,Coord),Solution), findfinalSol(Solution,Tail,Result).
findfinalSol(Solution,[Coord|Tail],Result) :- not member((_,Coord),Solution), findfinalSol(Solution,Tail,Result).
 
find_final_list([],[]).
find_final_list([[Word1|Coords1List]|Tail],[(Word1,Coord1)|Result]) :-
member(Coord1,Coords1List),
delAll(Word1,Coord1,Tail,UpdatedList),
quicksort(UpdatedList,NewSorted),
find_final_list(NewSorted,Result).

delAll(_,_,[],[]).
delAll(Word1,Coord1,[[Word2|Coord2List]|Tail],[[Word2|Final]|Res]) :- 
delMember(Word1,Coord1,Word2,Coord2List,Final),
delAll(Word1,Coord1,Tail,Res).

delMember(_,_,_,[],[]) :- !.
delMember(Word1,Coord1,Word2,[Coord2|Xs], Y) :- collision(Coord1,Word1,Coord2,Word2),!, 
delMember(Word1,Coord1,Word2, Xs, Y).
delMember(Word1,Coord1,Word2,[Coord2|Xs], [Coord2|Y]) :- not collision(Coord1,Word1,Coord2,Word2),!,
delMember(Word1,Coord1, Word2, Xs, Y).

match_words_to_spaces(R,S) :-
split_words_to_chars(C),
empty_space(S),
match2(C,S,R).
 
match2([],_,[]).
match2([Word|Tail],Spaces,[R|Result]) :- match_word(Word,Spaces,Res), append([Word],Res,R), match2(Tail,Spaces,Result).

match_word(_,[],[]).
match_word(Word,[Space|Tail],[Space|Rest]) :- same_length(Word,Space), match_word(Word,Tail,Rest).
match_word(Word,[Space|Tail],Rest) :- not same_length(Word,Space), match_word(Word,Tail,Rest).


same_length([],[]).
same_length([_|L1],[_|L2]) :- same_length(L1, L2).

split_words_to_chars(C) :- words(L), split_words(L,C).

split_words([],[]).
split_words([Head|Rest1],[Res|Rest2]) :- name(Head,L), split_chars(L,Res), split_words(Rest1,Rest2).
split_chars([],[]).
split_chars([Head|Rest1],[Res|Rest2]) :- name(Res,[Head]), split_chars(Rest1,Rest2).

empty_space(EmptySpace) :- 
dimension(Size), create_grid(Size,Grid),
findall(black(I,J),black(I,J),Blacks), 
fillblacks(Blacks,Grid),!,
transpose(Grid,Trans),
find_empty_space_rows(Grid,EmptySpace1),
find_empty_space_cols(Trans,EmptySpace2),
append(EmptySpace1,EmptySpace2,EmptySpace).
 

create_grid(Size,Grid) :- length(Grid,Size), create_lines(Grid,Size).

create_lines([],_).
create_lines([Head|Rest],Size) :- length(Head,Size), create_lines(Rest,Size). 

fillblacks([],_).
fillblacks([black(I,J)|Blacks],Grid) :- 
II is I - 1, match(Grid,II,Line), JJ is J - 1, match(Line,JJ,black), fillblacks(Blacks,Grid). 

match([H|_],0,H) :- !.
match([_|T],N,H) :- N > 0, N1 is N-1, match(T,N1,H).

find_empty_space_rows(Grid,EmptySpace) :- find_empty_space_inrows(Grid,EmptySpace,1,1),!.
find_empty_space_cols(Trans,EmptySpace) :- find_empty_space_incols(Trans,EmptySpace,1,1),!.

find_empty_space_inrows([],[],_,_).
find_empty_space_inrows([Row|Rest],EmptySpace,R,C) :- 
find1(Row,R,C,Res), pack(Res,PackedRes), delBlacks(PackedRes,FinRes), R1 is R + 1, find_empty_space_inrows(Rest,Space,R1,C),append(FinRes,Space,EmptySpace).  

find1([],_,_,[]).
find1([Head|Rest],R,C,[(R,C)|Temp]) :- Head = white, C1 is C + 1, find1(Rest,R,C1,Temp).
find1([_|Rest],R,C,[b(R,C)|Temp]) :- C1 is C + 1, find1(Rest,R,C1,Temp).

find_empty_space_incols([],[],_,_).
find_empty_space_incols([Row|Rest],EmptySpace,R,C) :- 
find2(Row,R,C,Res), pack(Res,PackedRes), delBlacks(PackedRes,FinRes), C1 is C + 1, find_empty_space_incols(Rest,Space,R,C1), append(FinRes,Space,EmptySpace).  

find2([],_,_,[]).
find2([Head|Rest],R,C,[(R,C)|Temp]) :- Head = white, R1 is R + 1, find2(Rest,R1,C,Temp).
find2([_|Rest],R,C,[b(R,C)|Temp]) :- R1 is R + 1, find2(Rest,R1,C,Temp).

pack([],[]).
pack([b(X,Y)|Xs],[Z|Zs]) :- transfer(b(X,Y),Xs,Ys,Z), pack(Ys,Zs).
pack([(X,Y)|Xs],[Z|Zs]) :- transfer((X,Y),Xs,Ys,Z), pack(Ys,Zs).
transfer(b(X,Y),[],[],[b(X,Y)]).
transfer((X,Y),[],[],[(X,Y)]).
transfer(b(X,Y),[(XX,YY)|Ys],[(XX,YY)|Ys],[b(X,Y)]).
transfer((X,Y),[b(XX,YY)|Ys],[b(XX,YY)|Ys],[(X,Y)]).
transfer((X,Y),[(XX,YY)|Xs],Ys,[(X,Y)|Zs]) :- transfer((XX,YY),Xs,Ys,Zs).
transfer(b(X,Y),[b(XX,YY)|Xs],Ys,[b(X,Y)|Zs]) :- transfer(b(XX,YY),Xs,Ys,Zs).

delBlacks([],[]).
delBlacks([[b(_,_)|_]|Rest],NewRes) :- delBlacks(Rest,NewRes).
delBlacks([Head|Rest],[Head|NewRes]) :- delBlacks(Rest,NewRes).

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
   
quicksort([], []).
quicksort([HEAD | TAIL], SORTED) :-
                                    len(HEAD,Len),partition(Len, TAIL, LEFT, RIGHT),
                                    quicksort(LEFT, SORTEDL),
                                    quicksort(RIGHT, SORTEDR),
                                    append(SORTEDL, [HEAD | SORTEDR], SORTED).
partition(_, [], [], []).
partition(PIVOT, [HEAD | TAIL], [HEAD | LEFT], RIGHT) :- len(HEAD,Len),	Len @=< PIVOT, partition(PIVOT, TAIL, LEFT, RIGHT).
partition(PIVOT, [HEAD | TAIL], LEFT, [HEAD | RIGHT]) :- len(HEAD,Len), Len @> PIVOT, partition(PIVOT, TAIL, LEFT, RIGHT).

len([], LenResult) :- LenResult is 0.
len([_|Y], LenResult):- len(Y, L), LenResult is L + 1.

collision(Coord1,Word1,Coord2,Word2) :- 
my_intersection(Coord1,Coord2,Common), member(Com,Common),
indexOf(Coord1,Com,N1), match(Word1,N1,Letter1), indexOf(Coord2,Com,N2), match(Word2,N2,Letter2), Letter1 \= Letter2.

my_intersection([],_,[]).	
my_intersection([X|Tail],Y,[X|Z]) :- member(X,Y), my_intersection(Tail,Y,Z).
my_intersection([X|Tail],Y,Z) :- \+ member(X,Y), my_intersection(Tail,Y,Z).

indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):- indexOf(Tail, Element, Index1), !, Index is Index1+1.



  


