insertAt(Element,0,L,[Element|L]).
insertAt(Element,Pos,[E|L],[E|ZL]):-
Pos1 is Pos-1,
insertAt(Element,Pos1,L,ZL).

delAll(_,[],[]).
delAll(Elem,[Y|Tail],[Final|Res]) :- 
delMember(Elem,Y,Final),
delAll(Elem,Tail,Res).

delMember(_, [], []).
delMember([(X,L1),R], [[(X,L2),RR]|Xs], Y) :- !,
delMember([(X,L1),R], Xs, Y).
delMember([(X,L1),R], [[(T,L2),RR]|Xs], [[(T,L2),RR]|Y]) :- !,
delMember([(X,L1),R], Xs, Y).

print_list([]).
print_list([Head|Tail]) :- writeln(Head), print_list(Tail).

pp([]).
pp([L|T]) :- print_list(L), writeln("--------------"),pp(T).

find_elem_possible_inserts(Pos,_,Max,_,_,[]) :- Pos =:= Max + 0.

find_elem_possible_inserts(Pos,Index,Max,Elem,List,NewL) :-
Pos =:= Index + 0, Pos \= Max, P is Pos + 1,
find_elem_possible_inserts(P,Index,Max,Elem,List,NewL).

find_elem_possible_inserts(Pos,Index,Max,Elem,List,[[(Elem,Pos),L]|NewL]) :-
Pos \= Index, Pos \= Max, P is Pos + 1, insertAt(Elem,Pos,List,L),
find_elem_possible_inserts(P,Index,Max,Elem,List,NewL).

find_all_possible_inserts(L,Result) :- length(L,N), lists_cre(L,L,0,N,Result).

lists_cre([],_,_,_,[]).
lists_cre([Elem|Tail],List,Index,Max,Res) :- delete(List,Elem,L), 
find_elem_possible_inserts(0,Index,Max,Elem,L,R1),
Ind is Index + 1, lists_cre(Tail,List,Ind,Max,R2), append(R1,R2,Res).

find_all_lists([],[]).
find_all_lists([L|Tail],[R|Res]) :- find_all_possible_inserts(L,R), find_all_lists(Tail,Res).

check(_,[]).
check([(_,_),List],[T|Tail]) :- member([(XX,YY),List],T), delAll([(XX,_),_],Tail,NewTail), check([(_,_),List],NewTail). 


findlist(L,List) :- find_all_lists(L,[First|Tail]),
member([(X,_),List],First),  
delAll([(X,_),_],Tail,NewTail),
check([(_,_),List],NewTail).  
