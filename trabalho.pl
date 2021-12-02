%----------------------------------------------------------------------------------------------
%--------------------- Inteligência Artificial -> Parte I -----------------------------
%----------------------------------------------------------------------------------------------

%----------------Elementos do Grupo -----------------------------
%------- Ricardo Miguel Santos Gomes, A93785
%------- Pedro Aquino Martins de Araújo, A90614
%------- Rui Pedro Gomes Coelho, A58898
%-------
%-----------------------------------------------------------------
%
% estafetas:					veiculos:
%	manuel							bicicleta --------> no máximo 5 kg, a 10 km/h
%	jose							mota -------------> no máximo 20 kg a 35 km/h
%	fabio							carro ------------> no máximo 100 kg a 25 km/h
%	marco
%
% clientes:						tipoEncomenda:
%	maria							roupa
%	ana								comida
%	filipa							moveis
%	cristina
%
%
% prazo:						classificacao:							ruas:
%	imediato -> 0h					0 a 5 estrelas							avenida da liberdade
%	24h																		rua direita
%	6h																		rua de campelo
%	1 dia -> 24h															travessa da igreja
%	...
%
%
% data:
%	11/10/2021
%	12/10/2021
%	13/10/2021
%	14/10/2021
%	15/10/2021
%
%




% Extensao do predicado entrega: estafeta, veiculo, km, tipoEncomenda, pesoEnc, prazo, velocidade, cliente, rua, classificacao, data encomenda, data entrega, preco


entrega(manuel, bicicleta, 12, comida, 2, 1, 10, maria, 'avenida da liberdade, braga', 4, 11/10/2021/10, 11/10/2021/11).
entrega(manuel, bicicleta, 12, comida, 2, 1, 10, maria, 'avenida da liberdade, braga', 4, 11/10/2021/13, 11/10/2021/14).
entrega(manuel, bicicleta, 12, comida, 2, 1, 10, maria, 'avenida da liberdade, braga', 4, 11/10/2021/16, 11/10/2021/18).

entrega(manuel, bicicleta, 12, comida, 2, 1, 10, maria, 'avenida da liberdade, braga', 4, 11/10/2021/12, 11/10/2021/13).
entrega(jose, bicicleta, 15, comida, 4, 1, 10, ana, 'rua direita, barcelos', 5, 11/10/2021/12, 11/10/2021/13).
entrega(fabio, mota, 20, roupa, 12, 24, 35, filipa, 'rua de campelo, guimaraes', 5, 12/10/2021/13, 12/10/2021/19).
entrega(marco, carro, 23, movel, 80, 48, 25, cristina, 'travessa da igreja, famalicao', 3, 11/10/2021/17, 14/10/2021/10).

entrega(jose, bicicleta, 15, comida, 4, 1, 10, ana, 'rua direita, barcelos', 5, 11/10/2021/20, 11/10/2021/21).
entrega(manuel, bicicleta, 15, comida, 1, 1, 10, ana, 'rua direita, barcelos', 5, 11/10/2021/19, 11/10/2021/20).
entrega(fabio, bicicleta, 15, comida, 2, 1, 10, ana, 'rua direita, barcelos', 3, 12/10/2021/21, 12/10/2021/22).


entrega(manuel, bicicleta, 15, comida, 1, 1, 10, ana, 'rua direita, barcelos', 5, 11/10/2021/13, 11/10/2021/15).
entrega(manuel, mota, 12, roupa, 10, 18, 35, maria, 'avenida da liberdade, braga', 3, 12/10/2021/11, 13/10/2021/10).
entrega(manuel, mota, 23, comida, 3, 0.5, 35, cristina, 'travessa da igreja, famalicao', 4, 12/10/2021/14, 12/10/2021/15).
entrega(manuel, carro, 20, movel, 74, 30, 25, filipa, 'rua de campelo, guimaraes', 5, 13/10/2021/14, 16/10/2021/10).
entrega(manuel, carro, 23, comida, 3, 0.5, 25, cristina, 'travessa da igreja, famalicao', 4, 15/10/2021/18, 15/10/2021/19).

entrega(jose, carro, 15, roupa, 17, 4, 25, ana, 'rua direita, barcelos', 2, 15/10/2021/18, 17/10/2021/10).
entrega(jose, mota, 12, comida, 2, 1, 35, maria, 'avenida da liberdade, braga', 3, 14/10/2021/11, 14/10/2021/12).
entrega(jose, mota, 20, comida, 3, 0.5, 35, filipa, 'rua de campelo, guimaraes', 1, 12/10/2021/10, 12/10/2021/12).
entrega(jose, carro, 20, movel, 74, 30, 25, filipa, 'rua de campelo, guimaraes', 5, 13/10/2021/9, 16/10/2021/10).
entrega(jose, carro, 15, movel, 60, 5, 25, ana, 'rua direita, barcelos', 3, 14/10/2021/15, 15/10/2021/10).

entrega(fabio, carro, 15, movel, 45, 17, 25, ana, 'rua direita, barcelos', 2, 15/10/2021/15, 18/10/2021/10).
entrega(fabio, mota, 12, roupa, 9, 5, 35, maria, 'avenida da liberdade, braga', 3, 12/10/2021/16, 14/10/2021/10).
entrega(fabio, mota, 20, comida, 4, 2, 35, filipa, 'rua de campelo, guimaraes', 1, 12/10/2021/21, 12/10/2021/22).
entrega(fabio, carro, 23, movel, 53, 25, 25, cristina, 'travessa da igreja, famalicao', 2, 13/10/2021/4, 15/10/2021/10).
entrega(fabio, bicicleta, 15, comida, 2, 1, 10, ana, 'rua direita, barcelos', 3, 12/10/2021/13, 12/10/2021/14).

entrega(marco, bicicleta, 15, roupa, 3, 1, 10, ana, 'rua direita, barcelos', 2, 14/10/2021/9, 14/10/2021/18).
entrega(marco, mota, 12, comida, 4, 0.5, 35, maria, 'avenida da liberdade, braga', 4, 12/10/2021/10, 12/10/2021/12).
entrega(marco, mota, 23, comida, 3, 0.5, 35, cristina, 'travessa da igreja, famalicao', 5, 12/10/2021/11, 12/10/2021/12).
entrega(marco, carro, 20, movel, 74, 30, 25, filipa, 'rua de campelo, guimaraes', 5, 13/10/2021/11, 16/10/2021/10).
entrega(marco, carro, 23, movel, 89, 23, 25, cristina, 'travessa da igreja, famalicao', 5, 15/10/2021/14, 19/10/2021/10).



:-dynamic estafetaRanking/2.

%classificação dos estafetas na empresa ----> serve para as penalizações
estafetaRanking(jose, 4.3).
estafetaRanking(marco, 4.1).
estafetaRanking(fabio, 3.8).
estafetaRanking(manuel, 4.5).






entregaValida(E, V, KM, T, P, Pr, Vel, C, R, Cla, Denc, Dent) :- entrega(E, V, KM, T, P, Pr, Vel, C, R, Cla, Denc, Dent),
	veiculo(V, P, Vel),
	classificacao(Cla),
	morada(C, R, KM),
	data(Denc),
	data(Dent).

% Extensao do predicado veiculo: veiculo, peso, velocidade -> {V,F}
veiculo(bicicleta, P, V) :-
	integer(P) =< 5, !,
	V =:= 10, !.
veiculo(mota, P, V) :-
	integer(P) =< 20, !,
	V =:= 35, !.
veiculo(carro, P, V) :-
	integer(P) =< 100, !,
	V =:= 25, !.


% Extensao do predicado classificacao: classificacao -> {V,F}

classificacao(C) :-
	member(C, [0,1,2,3,4,5]), !.


% Extensao do predicado morada: cliente, rua -> {V, F}

morada(maria, 'avenida da liberdade, braga', 12).
morada(ana, 'rua direita, barcelos', 15).
morada(filipa, 'rua de campelo, guimaraes', 20).
morada(cristina, 'travessa da igreja, famalicao', 23).




%data, mes, ano, hora
data(D/2/A/H) :-
    A >= 0, !,
	A mod 4 =:= 0, !,
	D >= 1, !,
	D =< 29, !,
	member(H, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]), !.
data(D/M/A/H) :-
	A >= 0, !,
    member(M, [1,3,5,7,8,10,12]), !,
	D >= 1, !,
	D =< 31, !,
	member(H, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]), !.
data(D/M/A/H) :-
	A >= 0, !,
    member(M, [4,6,9,11]), !,
	D >= 1, !,
	D =< 30, !,
	member(H, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]), !.
data(D/2/A/H) :-
	A >= 0, !,
    A mod 4 =\= 0, !,
	D >= 1, !,
	D =< 28, !,
	member(H, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]), !.




% 1) identificar o estafeta que utilizou mais vezes um meio de transporte mais ecológico;

% Extensao do predicado estafetaMaisVezesTransp: Veiculo, Estafeta -> {V,F}


%predicado para procurar todas as entregas feitas por determinado veiculo e guardar o nome do estafeta numa lista
estafetaMaisVezesTransp(V, R) :- findall(E, entregaValida(E, V, KM, T, P, Pr, Vel, C, R, Cla, Denc, Dent), [H|T]),
								maior(H, 1, [H|T], R).


%predicado para verificar qual o nome do estafeta aparece mais vezes numa lista
maior(Nome, N, [], Nome).

maior(Nome, N, [H|T], R) :- quantosIguais([H|T], N1),
							N >= N1,
							maior(Nome, N, T, R), !.

maior(H, N1, [H|T], R) :- quantosIguais([H|T], N1),
							N < N1,
							maior(H, N1, T, R), !.









% 2) identificar que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente;

% Extensao do predicado estafetasEntregasCliente: Cliente, Lista encomendas, Lista de estafetas -> {V,F}


%verifica se a entrega é valida e devolve o estafeta
estafetasEntregaClienteValida(C, T, E) :- entregaValida(E, V, KM, T, P, Pr, Vel, C, R, Cla, Denc, Dent).

%devolve uma lista com os estafetas que entregaram um tipo de encomenda a um determinado cliente
estafetasEntregaCliente(C, T, R) :- findall(E, estafetasEntregaClienteValida(C, T, E), L),
									retiraDup(L, [], R).

todasEntregasDup(C, [], []).

%concatena todas as listas que airem de estafetasEntregaCliente
todasEntregasDup(C, [T|Tail], R) :- estafetasEntregaCliente(C, T, Temp),
								concatenar(Temp, Temp1, R),
								todasEntregasDup(C, Tail, Temp1).

%executar esta
todasEntregas(C, T, R) :- todasEntregasDup(C, T, D),
						retiraDup(D, [], R), !.

%retira os duplicados presentes numa lista
retiraDup([], [], nenhum).
retiraDup([H|T], [], R) :- retiraDup(T, [H], R), !.

retiraDup([H|T], A, R) :- member(H, A),
						retiraDup(T, A, R), !.

retiraDup([H|T], [HA|TA], R) :- retiraDup(T, [H, HA|TA], R), !.

retiraDup([], L, L).








% 3) identificar os clientes servidos por um determinado estafeta

% Extensao do predicado clientesServidosEstafeta: Estafeta, Lista Clientes -> {V,F}


clientesServidosEstafeta(E, Clientes) :- findall(C, entregaValida(E, V, KM, T, P, Pr, Vel, C, R, Cla, Denc, Dent), L),
										retiraDup(L, [], Clientes), !.




% 4) calcular o valor faturado pela Green Distribution num determinado dia


%custo de 2/Kg para transportar de bicicleta + 0.15/km
custoTransporte(bicicleta, P, KM, Custo) :- Custo is ((2 * P) + (0.20 * KM)).

%custo de 4/Kg para transportar de bicicleta + 0.35/km
custoTransporte(mota, P, KM, Custo) :- Custo is ((4 * P) + (0.35 * KM)).

%custo de 8/Kg para transportar de bicicleta + 0.50/km
custoTransporte(carro, P, KM, Custo) :- Custo is ((8 * P) + (0.50 * KM)).


%devolve o veiculo, o peso e a distancia das entregas validas
encontraValores(D/M/A/_, (V, P, KM)) :- entregaValida(E, V, KM, T, P, Pr, Vel, C, R, Cla, Denc, D/M/A/_).


%executar
valorFaturado(D/M/A/_, R) :- findall((V, P, KM), encontraValores(D/M/A/_, (V, P, KM)), L),
							calculaValor(L, R).

%calcula o valor presente em cada triplo da lista e devolve o total
calculaValor([], 0).
calculaValor([(V, P, KM)|T], R) :- custoTransporte(V, P, KM, Custo),
									calculaValor(T, Custo2),
									R is Custo + Custo2, !.













% 5) identificar quais as zonas (e.g., rua ou freguesia) com maior volume de entregas por parte da Green Distribution
%volumeZona(Res).

volumeZona(Res) :- findall(Zone,entregaValida(E, V, KM, T, P, Pr, Vel, C, Zone, Cla, Denc, Dent), Zones),
				   countVol(Zones,Aux),
				   sort(2,@>=,Aux, Aux1),
				   head(Aux1,Res).

countVol(List, Occ):-
	findall([X,L], (bagof(true,member(X,List),Xs), length(Xs,L)), Occ).

head([], Res ).
head([H|_], Res) :- Res = H.






% 6) calcular a classificação média de satisfação de cliente para um determinado estafeta

classificacaoMedia(E, R) :- findall(Class, entregaValida(E, V, KM, T, P, Pr, Vel, C, Zone, Class, Denc, Dent), L),
							calculaMedia(L, R).


%devolve a média de classificações atribuidas pelos clientes a um determinado estafeta
calculaMedia(L, R) :- somatorio(L, S),
						comprimento(L, C),
						C > 0,
						R1 is S / C,
						round(R1, R, 1).


%arredonda para X para D casas decimais e devolve o resultado Y
round(X,Y,D) :- Z is X * 10^D, round(Z, ZA), Y is ZA / 10^D.













% identificar o número total de entregas pelos diferentes meios de transporte, num determinado intervalo de tempo;
% 7) Extensao do predicado

% função principal
totalVeiculoEntrega(Data1,Data2,Nentregas):- data(Data1),
											data(Data2),
											checkPeriodo(Data1,Data2),
											veiculos(Veiculos),
											calculaVDifEntrega(Data1,Data2,[],Veiculos, Nentregas), !.

% coloca em R a lista  de todos veículos
veiculos(R) :- findall(V, entregaValida(E, V, KM, T, P, Pr, Vel, C, Zone, Class, Denc, Dent), L), retiraDup(L, [], R).

% função auxiliar que utiliza outra lista (N) para colocar os novos tuplos ((Veiculo,nº de entregas feitas pelo veículo no período))
calculaVDifEntrega(Data1,Data2,N,[Vult],Nentregas):- Nentregas= [(Vult,X)|N],
													calculaVentrega(Vult,X,Data1,Data2).

calculaVDifEntrega(Data1,Data2, N , [Vatual,Vprox|Outros] , Nentregas) :- Novo = [(Vatual,X)|N],
															 calculaVentrega(Vatual,X,Data1,Data2),
                                                                         calculaVDifEntrega(Data1,Data2,Novo,[Vprox|Outros],Nentregas).

% função auxiliar que calcula o número total de entregas feitas por um veículo dentro do período D1 / D2
calculaVentrega(Veiculo,N,D1,D2):- findall(_,(entregaValida(E, Veiculo, KM, T, P, Pr, Vel, C, Zone, Class, Denc, D),checkData(D1,D2,D)),L),
									length(L,N).








%identificar o número total de entregas pelos estafetas, num determinado intervalo de tempo;
% 8) Extensao do predicado

% função principal
totalEntregasEstafetas(Data1,Data2,Nentregas):- data(Data1),
												data(Data2),
												checkPeriodo(Data1,Data2),
												estafetas(Estafetas),
												calculaEstafEntregas(Data1,Data2,[],Estafetas, Nentregas), !.

% coloca em R a lista  de todas estafetas
estafetas(R) :- findall(E, entregaValida(E, V, KM, T, P, Pr, Vel, C, Zone, Class, Denc, Dent), L), retiraDup(L, [], R).


% função auxiliar que utiliza outra lista (N) para colocar os novos tuplos ((Estafeta,nº de entregas realizada no período))
calculaEstafEntregas(Data1,Data2,N,[Eult],Nentregas):-
											Nentregas = [(Eult,X)|N],
											calculaEentrega(Eult,X,Data1,Data2).

calculaEstafEntregas(Data1,Data2, N , [Eatual,Eprox|Outros] , Nentregas) :-
																		  Novo = [(Eatual,X)|N],
																		  calculaEentrega(Eatual,X,Data1,Data2),
																		  calculaEstafEntregas(Data1,Data2,Novo,[Eprox|Outros],Nentregas).


% função auxiliar que calcula o número total de entregas feitas por uma estafeta dentro do período D1 / D2
calculaEentrega(E,N,D1,D2):- findall(_,(entregaValida(E, V, KM, T, P, Pr, Vel, C, Zone, Class, Denc, D),checkData(D1,D2,D)),L), length(L,N).












% 9)  calcular o número de encomendas entregues e não entregues pela Green Distribution, num determinado período de tempo;

% Verifica a entrega está dentro do período estipulado
verificaPeriodo(D1, D2, (P, DEnc, DEnt)) :- entrega(_,_,_,_,_,P,_,_,_,_,DEnc, DEnt),checkData(D1, D2, DEnt).


% Função principal
numEncomendas(D1, D2, Ent, NEnt) :- findall((Tent, TNent),(verificaPeriodo(D1, D2, (P, DEnc, DEnt)),periodoEmHoras((DEnc,DEnt),P2) ,foiEntregue((P,P2),Tent,TNent)), L),contaEncomendas(L,0,0,Ent, NEnt).


% calcula as horas de uma data
calculaHoras(D/2/A/H,X) :- A mod 4 =:= 0,calculaHorasDia(D,W), calculaHorasDia(29,Y), calculaHorasDia(366,Z), X is (Y*2) + (Z*A) + W + H,!.
calculaHoras(D/2/A/H,X) :- calculaHorasDia(D,W),calculaHorasDia(28,Y) ,calculaHorasDia(365,Z), X is (Y*2) + (Z*A) + W + H,!.
calculaHoras(D/M/A/H,X):- member(M, [1,3,5,7,8,10,12]),calculaHorasDia(D,W), calculaHorasDia(31,Y), calculaHorasDia(365,Z), X is (Y*M) + (Z*A) + H + W,!.
calculaHoras(D/M/A/H,X):- calculaHorasDia(D,W),calculaHorasDia(30,Y), calculaHorasDia(365,Z), X is (Y*M) + (Z*A) +W+ H,!.


% calcula horas do dia
calculaHorasDia(D,X) :- X is D * 24.

% Faz a diferença de horas entre duas datas
periodoEmHoras((DEnc,DEnt),X):- calculaHoras(DEnt,Tent), calculaHoras(DEnc,Tenc), X is Tent - Tenc.



% Verifica aquelas entregas que passaram do prazo e as que não
foiEntregue((P,P2),Tent,Nent):- P2 > P , Tent is 0 , Nent is 1,!.
foiEntregue(_,Tent,Nent):- Tent is 1 , Nent is 0.





% Contabiliza o número total de entregas e não entregas
contaEncomendas([], Ent, NEnt, Ent, NEnt).
contaEncomendas([(E,_)|T], AcEnt, AcNEnt, Ent, NEnt) :- E =:= 1 , NewEnt is AcEnt + 1, contaEncomendas(T,NewEnt,AcNEnt,Ent,NEnt),!.
contaEncomendas([_|T], AcEnt, AcNEnt, Ent, NEnt) :- NewNEnt is AcNEnt + 1, contaEncomendas(T,AcEnt,NewNEnt,Ent,NEnt).

















% 10) calcular o peso total transportado por estafeta num determinado dia

% devolve um tuplo com o estafeta e o peso transportado num determinado dia
pesoTransEstafeta(D/M/A/_, (E, P)) :- entregaValida(E, V, KM, T, P, Pr, Vel, C, Zone, Class, Denc, D/M/A/_).

%executar esta
pesoTransEstafetaDia(D/M/A/_, R) :- findall((E, P), pesoTransEstafeta(D/M/A/_, (E, P)), L),
									agrupa(L, [], R).


%agrupa em tuplos os estafetas repetidos, somando os valores transportados
agrupa([], R, R).
agrupa([H|T], [], R) :- agrupa(T, [H], R), !.

agrupa([H1|T1], [H2|T2], R) :- pertenceC(H1, [H2|T2]),
								atualizaPesos(H1, [H2|T2], A),
								agrupa(T1, A, R), !.

agrupa([H1|T1], [H2|T2], R) :- agrupa(T1, [H1, H2|T2], R), !.



%soma os valores transportados se os estafetas presentes nos tuplos forem iguais
atualizaPesos(X, [], []).
atualizaPesos((Nome1, Peso1), [(Nome1, Peso2)|T2], [(Nome1, PesoTotal)|A]) :-  PesoTotal is Peso1 + Peso2,
																				atualizaPesos((Nome1, Peso1), T2, A), !.

atualizaPesos((Nome1, Peso1), [(Nome2, Peso2)|T2], [(Nome2, Peso2)|A]) :- Nome1 \= Nome2,
																			atualizaPesos((Nome1, Peso1), T2, A), !.





pertenceC( (X, P),[(X, N)|L] ).
pertenceC( (X, P),[(Y, N)|L] ) :-
    X \= Y,
    pertenceC( (X, P),L ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado somatorio: Lista,Somatorio -> {V,F}

somatorio([], 0).
somatorio([H|T], R) :- somatorio(T, R2),
						R is H + R2.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado comprimento: Lista,Comprimento -> {V,F}

comprimento( [],0 ).
comprimento( [X|L],N ) :-
    comprimento( L,N1 ),
    N is N1+1.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado concatenar: Lista1,Lista2,Resultado -> {V,F}

concatenar(L1, [], L1).
concatenar([], L2, L2).
concatenar([H1|T1], [H2|T2], [H1|L]) :-
	concatenar(T1, [H2|T2], L).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).


%-----------------------------------------------
% Extensao do predicado checkData que verifica se uma data pertence à um período de tempo: Data1,Data2,Data -> {V,F}.
checkData( D1/M1/A1/H1 , D1/M1/A1/H2 , D1/M1/A1/H ):- !,H >= H1,!,H =< H2, !.

checkData( _ , D2/M1/A1/H2 , D2/M1/A1/H ):- !,H =< H2,!.
checkData( D1/M1/A1/H1 , _ , D1/M1/A1/H ):- !,H >= H1,!.

checkData( D1/M1/A1/_ , D2/M1/A1/_ , D/M1/A1/_ ):- !,D >= D1,!,D =< D2,!.

checkData( _ , D2/M1/A1/_ , D/M1/A1/_ ):- !,D =< D2,!.
checkData( D1/M1/A1/_ , _ , D/M1/A1/_ ):- !,D >= D1,!.

checkData( _/M1/A1/_ , _/M2/A1/_ , _/M/A1/_ ):- !,M >= M1,!,M =< M2,!.

checkData( _ , _/M2/A1/_ , _/M/A1/_ ):- !,M =< M2,!.
checkData( _/M1/A1/_ , _ , _/M/A1/_ ):- !,M >= M1,!.

checkData( _/_/A1/_ , _/_/A2/_ , _/_/A/_ ):- !,A >= A1,!,A =< A2,!.

%-----------------------------------------------
% Extensao do predicado checkPeriodo , que verifica se o periodo é válido: Data1,Data2 -> {V,F}
checkPeriodo(D1/M1/A1/H1 , D1/M1/A1/H2) :- !,H1 =< H2,!.
checkPeriodo(D1/M1/A1/_ , D2/M1/A1/_) :- !,D1 < D2,!.
checkPeriodo(_/M1/A1/_ , _/M2/A1/_) :- !,M1 < M2,!.
checkPeriodo(_/_/A1/_ , _/_/A2/_) :- !,A1 < A2,!.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Extensao do predicado quantos iguais: Lista,Comprimento -> {V,F}



quantosIguais([], 0).
quantosIguais([H|T], N) :- member(H,T),
	quantosIguais(T,N1),
	N is N1 + 1.
quantosIguais([H|T], 1).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado apagatudo: Elemento,Lista,Resultado -> {V,F}

apagatudo(X, [], []).
apagatudo(X, [X|R], L) :-
	apagatudo(X, R, L).
apagatudo(X, [Y|R], [Y|L]) :-
	X \= Y,
	apagatudo(X, R, L).
