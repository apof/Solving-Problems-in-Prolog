frogs(N,M,Sol) :- depth_first_search(_,N,M,Sol).

depth_first_search(States,N,M,Sol) :- initial_state(State,N,M), depth_first_search(State, [State], States,N,M,Sol).

depth_first_search(State, States, States,N,M,[]) :- final_state(State,N,M).

depth_first_search(State1, SoFarStates, States,N,M,[Move|Sol]) :-
move(Move,State1, State2),
not member(State2, SoFarStates),
append(SoFarStates, [State2], NewSoFarStates),
depth_first_search(State2, NewSoFarStates, States,N,M,Sol).

initial_state(State,N,M) :-
append_green(N,Green),
append_brown(M,Brown), !,
append(Green,[e|Brown],State).

final_state(State,N,M) :-
append_green(N,Green),
append_brown(M,Brown), !,
append(Brown,[e|Green],State).

append_green(0,[]).
append_green(N,[g|State]) :- N1 is N - 1, append_green(N1,State).
append_brown(0,[]).
append_brown(N,[b|State]) :- N1 is N - 1, append_brown(N1,State).

move(g1,S1,S2) :- append(L1,[g,e|L2],S1), append(L1,[e,g|L2],S2).
move(b1,S1,S2) :- append(L1,[e,b|L2],S1), append(L1,[b,e|L2],S2).
move(g2,S1,S2) :- append(L1,[g,b,e|L2],S1), append(L1,[e,b,g|L2],S2).
move(b2,S1,S2) :- append(L1,[e,g,b|L2],S1), append(L1,[b,g,e|L2],S2).







