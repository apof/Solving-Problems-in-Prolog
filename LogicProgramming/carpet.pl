print_solution([]).
print_solution([X|Sol]) :- print_sublist(X), writeln(" "), print_solution(Sol).
print_sublist([]).
print_sublist([X|Sublist]) :-  write(X), print_sublist(Sublist).

carpet(N) :- N\=1, rewrite(_,List), construct(List,_,N).
carpet(N) :- N=1, rewrite(_,List), print_solution(List).

construct([],L,N) :- N\=1, N1 is N - 1, construct(L,_,N1).
construct(L,L,N) :- N=1, print_solution(L).
construct([List|Rest],Sol_all1,N) :- N\=1,
sublist_construct(List,_,Sol_all1,Sol_all2), !, construct(Rest,Sol_all2,N).

sublist_construct([],L,Sol_all1,Sol_all2) :- append(Sol_all1,L,Sol_all2).
sublist_construct([Elem|Rest],Sol,Sol_all1,Sol_all2) :- 
rewrite(Elem,List), sublist_append(Sol,List,Sol2), !, sublist_construct(Rest,Sol2,Sol_all1,Sol_all2).

sublist_append([],[],[]).
sublist_append([El1|Sol1],[El2|List],[Fin|Sol2]) :- append(El1,El2,Fin), sublist_append(Sol1,List,Sol2). 