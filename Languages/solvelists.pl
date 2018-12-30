zip([],[],[]).
zip([Head1|Tail1],[Head2|Tail2],[(Head1,Head2)|Tail]) :- zip(Tail1,Tail2,Tail).

cre_var_list([],[]).
cre_var_list([_|T],[(X,Y)|Tail]) :- cre_var_list(T,Tail).

var_assign([],[]).
var_assign([(X,Y)|Tail1],[(V1,V2)|Tail2]) :- integer(X),integer(Y),V1=X,V2=Y,var_assign(Tail1,Tail2).
var_assign([(X,Y)|Tail1],[(V1,V2)|Tail2]) :- integer(X),not(integer(Y)),V1=X,assign(Y,V2,Tail1,Tail2),var_assign(Tail1,Tail2).
var_assign([(X,Y)|Tail1],[(V1,V2)|Tail2]) :- not(integer(X)),integer(Y),V2=Y,assign(X,V1,Tail1,Tail2),var_assign(Tail1,Tail2).
var_assign([(X,Y)|Tail1],[(V1,V2)|Tail2]) :- not(integer(X)),not(integer(Y)),assign(Y,V2,Tail1,Tail2),assign(X,V1,Tail1,Tail2),var_assign(Tail1,Tail2).

assign(_,_,[],[]).
assign(X,V,[(A,B)|Tail1],[(C,_)|Tail2]) :- X=A, X\=B, C=V, assign(X,V,Tail1,Tail2).
assign(X,V,[(A,B)|Tail1],[(_,D)|Tail2]) :- X=B, X\=A, D=V, assign(X,V,Tail1,Tail2).
assign(X,V,[(A,B)|Tail1],[(C,D)|Tail2]) :- X=A, X=B, C=V, D=V, assign(X,V,Tail1,Tail2).
assign(X,V,[(A,B)|Tail1],[(_,_)|Tail2]) :- X\=A, X\=B, assign(X,V,Tail1,Tail2).

eq([]).
eq([(X,Y)|Tail]) :- not(var(X)),not(var(Y)), X=Y, eq(Tail).
eq([(X,Y)|Tail]) :- not(var(X)),var(Y), Y=X, eq(Tail).
eq([(X,Y)|Tail]) :- var(X),not(var(Y)), X=Y, eq(Tail).
eq([(X,Y)|Tail]) :- var(X),var(Y),eq(Tail).

find_Sol([],[],[]).
find_Sol([(X,Y)|Tail1],[(_,_)|Tail2],Sol) :- integer(X), integer(Y),find_Sol(Tail1,Tail2,Sol).
find_Sol([(X,Y)|Tail1],[(V1,V1)|Tail2],[(Y,V1)|Sol]) :- integer(X), not(integer(Y)),find_Sol(Tail1,Tail2,Sol).
find_Sol([(X,Y)|Tail1],[(V1,V1)|Tail2],[(X,V1)|Sol]) :- not(integer(X)), integer(Y),find_Sol(Tail1,Tail2,Sol).
find_Sol([(X,Y)|Tail1],[(V1,V1)|Tail2],[(X,V1),(Y,V1)|Sol]) :- not(integer(X)), not(integer(Y)),find_Sol(Tail1,Tail2,Sol).

solvelists(L1,L2,Sol) :- 
zip(L1,L2,L), 
cre_var_list(L,V),
var_assign(L,V),
eq(V),
find_Sol(L,V,S),
sort(S,Sol).

 


