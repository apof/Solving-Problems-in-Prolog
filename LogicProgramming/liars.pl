%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% xrhsimopoieitai h generate and test methodos					        %
% gia dedomenh lista mhkous N paragontai oi (2^N) pithanes lyseis tou provlhmatos       %
% kai gia kathe mia elegxetai an isxyoun oi proypotheseis wste telika na einai lysh     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

% dinei thn telikh lysh sto provlhma

liars(List,Result) :- list_length(List,Length), generate(Length,Result), liars_sum(Result,0,Liars), check(List,Result,Liars,0).

% dhmiourgei oles tis pithanes listes mhkous N pou apotelountai apo 0 kai 1, oi opoies einai kai oi pithanes lyseis tou provlhmatos

generate(0,[]).
generate(N,[0|T]) :- N > 0, Y is N-1, generate(Y,T).
generate(N,[1|T]) :- N > 0, Y is N-1, generate(Y,T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% vriskei to megethos mias listas

list_length([] , 0 ).
list_length([_|Xs] , L ) :- list_length(Xs,N) , L is N+1 .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% elegxw ama mia pithanh lysh einai dekth 
% eleghontas ama isxyoun oi periorismoi gia kathe ena stoixeio ths arxikhs listas se sxesh me thn ypo dokimh pithanh lista lysh 

% elegxw an h lista solution epalhtheyetai gia kathe atomo opote kai h test epistrefei 0
% an estw kai gia ena atomo den epalhtheyetai h test epistrefei 1 
% otan epistrafei 1 kaleitai kathgorhma to opoio kalei to teliko kathgorhma to opoio apotygxanei 0\=1
% mono otan gia ola ta stoixeia epistrafei 0 0=0 to teliko kathgorhma epitygxanei

check([],[],_,Result) :- Result = 0.
check(_,_,Liars,Res) :- Res = 1, check([],[],Liars,Res). 
check([X|List],[Y|Sol],Liars,Res) :- test(X,Y,Liars,Res), check(List,Sol,Liars,Res). 

% analogws me thn apopsh tou atomou to plhthos twn pseytwn kai thn timh tou atomou sth lista solution h lista ginetai dekth h oxi gia to sygekrimeno atomo

test(X,Y,Count,1) :- Count >= X, Y=1.
test(X,Y,Count,1) :- Count < X, Y=0.
test(X,Y,Count,0) :- Count < X, Y=1.
test(X,Y,Count,0) :- Count >= X, Y=0.

% elegxei posoi einai oi pseytes se mia pithanh lysh

liars_sum([],Count,Count).
liars_sum([A|Solut],Count,C) :- A=1, C1 is Count + 1, liars_sum(Solut,C1,C).
liars_sum([A|Solut],Count,C) :- A\=1, liars_sum(Solut,Count,C).