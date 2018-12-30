print_list([]).
print_list([Head|Tail]) :- writeln(Head), print_list(Tail).

remove_first_X((X,_),[(X,_)|Xs],Xs).
remove_first_X((X,A),[(Y,B)|Xs],[(Y,B)|Ys]) :- 
    X \= Y,
    remove_first_X((X,A),Xs,Ys).

create_init(_,[],[]).
create_init(N,[H|T],[(N,H)|Res]) :- NN is N + 1, create_init(NN,T,Res).

add_mitsos(L,[(1,0)|L]).

create_chain(_,[],[]).
create_chain((Id1,_),[(Id2,Y)|Rest],[Id2|Res]) :- Y =:= Id1 + 0, create_chain((Id2,Y),Rest,Res).
create_chain((Id1,_),[(_,Y)|Rest],Res) :- Y \= Id1, create_chain((Id1,_),Rest,Res).

create_all_chains(_,[],[]).
create_all_chains(Elem,List,[[1,First|Tail]|Res]) :-
create_chain(Elem,List,[First|Tail]),
last([First|Tail],Last),
remove_first_X((Last,_),List,List2),
create_all_chains(Elem,List2,Res).


create_costs([],[]).
create_costs([Ch1|Tail],Result) :- length(Ch1,N), cre_cost(Ch1,R1,N), create_costs(Tail,R2), append(R1,R2,Result).

cre_cost([],[],0).
cre_cost([Ind|Tail],[(Ind,N)|Res],N) :- NN is N - 1, cre_cost(Tail,Res,NN).

calculate_costs(_,0,[]).
calculate_costs(Costs,N,[C|Result]) :- 
findall(Cost,member((N,Cost),Costs),Cost_List), 
sumlist(Cost_List,C),
NN is N - 1,
calculate_costs(Costs,NN,Result).

corporation(L,[FF|Res1]) :- 
create_init(2,L,S),
add_mitsos(S,[Mitsos|Rest]),
create_all_chains(Mitsos,Rest,Chains),
create_costs(Chains,Costs),
length(L,N),
NN is N + 1,
calculate_costs(Costs,NN,R),
reverse(R,[First|Res1]),
FF is First + 1.


