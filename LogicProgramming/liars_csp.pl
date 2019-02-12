:-lib(ic).
:-lib(ic_global).

liars_csp(List,Result) :- 
length(List,Num),
length(Result,Num),
Result #:: 0..1,
Sum #:: 0..Num,
constrain(List,Result,Sum),
sumlist(Result,Sum),
search(Result,0,input_order,indomain_middle,complete,[]).

constrain([],[],_).
constrain([X|Tail1],[Y|Tail2],Sum) :-
Y #= (X #> Sum),
constrain(Tail1,Tail2,Sum).


genrand(N, List) :-
length(List, N),
make_list(N, List).
make_list(_, []).
make_list(N, [X|List]) :-
random(R),
X is R mod (N+1),
make_list(N, List).
