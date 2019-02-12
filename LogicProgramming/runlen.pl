% decoding % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 
% epe3ergazomaste ena ena ta stoixeia ths arxikhs listas                                         %
% h morfh tous einai (Char,Num) me char to xarakthra kai Num to plhthos twn adigrafwn tou        %
% opote kaloume th make ftiaxnei mia lista me tosa stoixeia Char osa ypodiknyei to Num           %
% h decode kaleitai anadromika gia ola ta stoixeia ths listas mas                                %
% ginontai append oles oi epimerous listes se mia telikh lista result                            %
% kai etsi prokyptei to teliko apotelesma                                                        %
% ama exoume mono stoixeio kaleitai to teleytaio decode pou apla to prosthetei sth lista         %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

make( _ , Number , [] ) :- Number =< 0 .
make( Char , Number , [Char|Result]) :- Number >= 1 , NUM is Number - 1 , make( Char , NUM , Result) .

decode_r1([],[]).
decode_r1([(Char,Num)|Rest],Result)  :- !, make(Char,Num,R1) , decode_r1(Rest,R2) , append(R1,R2,Result) .
decode_r1([Char|Rest],Result)  :- make(Char,1,R1) , decode_r1(Rest,R2) , append(R1,R2,Result) .

% encoding % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 
% X|L h lista me kefalh X pou epe3ergazomaste                                                                                                                    %
% Result h lista apotelesmatos kai Y to stoixeio pou kathe fora prosthetume                                                                                      %
% o counter ypologizei to stoixeio Y ekeino pou tha prosthesoume sto Result, Res h lista pou menei ama afairesoume apo thn L ola ta adigrafa tou X               %
% sto (X,Num) to Num einai o arithmos twn adigrafwn tou xarakthra X, arxika einai iso me 1 ama parameinei 1 (N=1) anti gia (X,1) prosthetoume apla X             %
% oi diaforetikoi orismoi gia ta counter kalyptoun tis periptwseis ta stoixeia pou theloume na exoun 1 opu apla epistrefetai to X, h pollapla adigrafa opou      %
% epistrefetai (X,Num)                                                                                                                                           %
% Epe3hghsh kata seira gia ta counter:                                                                                                                           %
% - to teleytaio stoixeio ( dhl otan h lista adeiasei ) einai mono tou (Num=1), den exei adigrafo opote epistrefoume X                                           %
% - to teleytaio stoixeio ( dhl otan h lista adeiasei ) exei adigrafa prin (Num>1) opote epistrefoume (X,Num)                                                    %
% - to stoixeio pou e3etazoume diaferei apo epomeno tou kai  den exei adigrafo opote gyrname X                                                                   %
% - to stoixeio pou e3etazoume diaferei apo epomeno tou alla exei adigrafa prin opote gyrname (X,Num)                                                            %
% - h vasikh anadromh pou kaloume oso to stoixeio tautizetai me to epomeno tou ay3anontas ton arithmo Num twn adigrafwn se kathe klhsh                           %  
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 

encode_r1([],[]).
encode_r1([X|L],[Y|Result]) :- counter(X,L,Res,1,Y), encode_r1(Res,Result).

counter(X,[],[],Num,X) :- Num=1.
counter(X,[],[],Num,(X,Num)) :- Num > 1.
counter(X,[R|Rest],[R|Rest],Num,X) :- Num=1, X\=R.
counter(X,[R|Rest],[R|Rest],Num,(X,Num)) :- Num > 1, X\=R.
counter(X,[Next|L],Result,Start_Num,B) :- X=Next, C1 is Start_Num + 1, counter(X,L,Result,C1,B).

% Apanthsh  sto erwthma: ?- encode_rl([p(3),p(X),q(X),q(Y),q(4)], L).% % % % % %
% X = 3                                                                        %
% Y = 3                                                                        %
% L = [(p(3), 2), (q(3), 2), q(4)]                                             %
%                                                                              %
% to apotelesma einai logiko % % % % % % % % % % % % % % % % % % % % % % % % % % 
% arxika ginetai sygrish tou p(3) me to p(X) to opoio gia X = 3 ikanopoieitai  %
% to X exei lavei thn timh 3                                                   %
% to p(3)/=q(3) ara (p(3),2) mpainei sth lista                                 %
% ginetai sygrish tou q(3) me q(Y) to opoio ikanopoeitai gia Y=3               %
% to Y lamvanei thn timh 3                                                     %
% to q(3)/=q(4) ara (p(3),2) mpainei sth lista                                 %
% telos mpainei sth lista kai to q(4)                                          %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 
