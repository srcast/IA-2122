%----------------------------------------------------------------------------------------------
%--------------------- Inteligência Artificial -> Parte I -----------------------------
%----------------------------------------------------------------------------------------------

%----------------Elementos do Grupo -----------------------------
%------- Ricardo Miguel Santos Gomes, A93785
%------- Pedro Aquino Martins de Araújo, A90614
%-------
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




% Extensao do predicado entrega: estafeta, veiculo, tipoEncomenda, pesoEnc, prazo, velocidade, cliente, rua, classificacao, data encomenda, data entrega, preco


entrega(manuel, bicicleta, comida, 2, 1, 10, maria, 'avenida da liberdade, braga', 4, 11/10/2021/10, 11/10/2021/11).
entrega(manuel, bicicleta, comida, 2, 1, 10, maria, 'avenida da liberdade, braga', 4, 11/10/2021/13, 11/10/2021/14).
entrega(manuel, bicicleta, comida, 2, 1, 10, maria, 'avenida da liberdade, braga', 4, 11/10/2021/16, 11/10/2021/18).








entrega(manuel, bicicleta, comida, 2, 1, 10, maria, 'avenida da liberdade, braga', 4, 11/10/2021/12, 11/10/2021/13).
entrega(jose, bicicleta, comida, 4, 1, 10, ana, 'rua direita, barcelos', 5, 11/10/2021/12, 11/10/2021/13).
entrega(fabio, mota, roupa, 12, 24, 35, filipa, 'rua de campelo, guimaraes', 5, 12/10/2021/13, 12/10/2021/19).
entrega(marco, carro, movel, 80, 48, 25, cristina, 'travessa da igreja, famalicao', 3, 11/10/2021/17, 14/10/2021/10).

entrega(jose, bicicleta, comida, 4, 1, 10, ana, 'rua direita, barcelos', 5, 11/10/2021/20, 11/10/2021/21).
entrega(manuel, bicicleta, comida, 1, 1, 10, ana, 'rua direita, barcelos', 5, 11/10/2021/19, 11/10/2021/20).
entrega(fabio, bicicleta, comida, 2, 1, 10, ana, 'rua direita, barcelos', 3, 12/10/2021/21, 12/10/2021/22).


entrega(manuel, bicicleta, comida, 1, 1, 10, ana, 'rua direita, barcelos', 5, 11/10/2021/13, 11/10/2021/15).
entrega(manuel, mota, roupa, 10, 18, 35, maria, 'avenida da liberdade, braga', 3, 12/10/2021/11, 13/10/2021/10).
entrega(manuel, mota, comida, 3, 0.5, 35, cristina, 'travessa da igreja, famalicao', 4, 12/10/2021/14, 12/10/2021/15).
entrega(manuel, carro, movel, 74, 30, 25, filipa, 'rua de campelo, guimaraes', 5, 13/10/2021/14, 16/10/2021/10).
entrega(manuel, carro, comida, 3, 0.5, 25, cristina, 'travessa da igreja, famalicao', 4, 15/10/2021/18, 15/10/2021/19).

entrega(jose, carro, roupa, 17, 4, 25, ana, 'rua direita, barcelos', 2, 15/10/2021/18, 17/10/2021/10).
entrega(jose, mota, comida, 2, 1, 35, maria, 'avenida da liberdade, braga', 3, 14/10/2021/11, 14/10/2021/12).
entrega(jose, mota, comida, 3, 0.5, 35, filipa, 'rua de campelo, guimaraes', 1, 12/10/2021/10, 12/10/2021/12).
entrega(jose, carro, movel, 74, 30, 25, filipa, 'rua de campelo, guimaraes', 5, 13/10/2021/9, 16/10/2021/10).
entrega(jose, carro, movel, 60, 5, 25, ana, 'rua direita, barcelos', 3, 14/10/2021/15, 15/10/2021/10).

entrega(fabio, carro, movel, 45, 17, 25, ana, 'rua direita, barcelos', 2, 15/10/2021/15, 18/10/2021/10).
entrega(fabio, mota, roupa, 9, 5, 35, maria, 'avenida da liberdade, braga', 3, 12/10/2021/16, 14/10/2021/10).
entrega(fabio, mota, comida, 4, 2, 35, filipa, 'rua de campelo, guimaraes', 1, 12/10/2021/21, 12/10/2021/22).
entrega(fabio, carro, movel, 53, 25, 25, cristina, 'travessa da igreja, famalicao', 2, 13/10/2021/4, 15/10/2021/10).
entrega(fabio, bicicleta, comida, 2, 1, 10, ana, 'rua direita, barcelos', 3, 12/10/2021/13, 12/10/2021/14).

entrega(marco, bicicleta, roupa, 3, 1, 10, ana, 'rua direita, barcelos', 2, 14/10/2021/9, 14/10/2021/18).
entrega(marco, mota, comida, 4, 0.5, 35, maria, 'avenida da liberdade, braga', 4, 12/10/2021/10, 12/10/2021/12).
entrega(marco, mota, comida, 3, 0.5, 35, cristina, 'travessa da igreja, famalicao', 5, 12/10/2021/11, 12/10/2021/12).
entrega(marco, carro, movel, 74, 30, 25, filipa, 'rua de campelo, guimaraes', 5, 13/10/2021/11, 16/10/2021/10).
entrega(marco, carro, movel, 89, 23, 25, cristina, 'travessa da igreja, famalicao', 5, 15/10/2021/14, 19/10/2021/10).


%entregaValida(E, V, T, P, Pr, Vel, C, R, Cla, Denc, Dent) :-
%	veiculo(V, P, Vel),
%	classificacao(Cla),
%	morada(C, R),
%	data(Denc),
%	data(Dent).

% Extensao do predicado veiculo: veiculo, peso, velocidade -> {V,F}
veiculo(bicicleta, P, V) :-
	integer(P) =< 5, 
	V =:= 10.
veiculo(mota, P, V) :-
	integer(P) =< 20,
	V =:= 35.
veiculo(carro, P, V) :-
	integer(P) =< 100,
	V =:= 25.


% Extensao do predicado classificacao: classificacao -> {V,F}

classificacao(C) :-
	pertence(C, [0,1,2,3,4,5]).


% Extensao do predicado morada: cliente, rua -> {V, F}

morada(maria, 'avenida da liberdade, braga').
morada(ana, 'rua direita, barcelos').
morada(filipa, 'rua de campelo, guimaraes').
morada(cristina, 'travessa da igreja, famalicao').




%data, mes, ano, hora
data(D/2/A/H) :-
    A >= 0,
	A mod 4 =:= 0,
	D >= 1,
	D =< 29,
	pertence(H, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]).
data(D/M/A/H) :-
	A >= 0,
    pertence(M, [1,3,5,7,8,10,12]),
	D >= 1,
	D =< 31,
	pertence(H, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]).
data(D/M/A/H) :-
	A >= 0,
    pertence(M, [4,6,9,11]),
	D >= 1,
	D =< 30,
	pertence(H, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]).
data(D/2/A/H) :-
	A >= 0,
    A mod 4 =\= 0, 
	D >= 1,
	D =< 28,
	pertence(H, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]).




% 1) identificar o estafeta que utilizou mais vezes um meio de transporte mais ecológico;

% Extensao do predicado estafetaMaisVezesTransp: Veiculo, Estafeta -> {V,F}

aux(V, E) :- entrega(E, V, _, _, _, _, _, _, _, _, _).

estafetaMaisVezesTransp(V, R) :- findall(E, entrega(E, V, _, _, _, _, _, _, _, _, _), [H|T]),
								%contaEstafetas(L, [], R).
								maior(H, 1, [H|T], R).


maior(Nome, N, [], Nome).

maior(Nome, N, [H|T], R) :- quantosIguais([H|T], N1),
							N >= N1,
							maior(Nome, N, T, R), !.

maior(H, N1, [H|T], R) :- quantosIguais([H|T], N1),
							N < N1,
							maior(H, N1, T, R), !.







%contaEstafetas([], [], nenhum). %talvez não seja necessário

%contaEstafetas([H|T], [], R) :- contaEstafetas(T, [(H, 1)], R).

%contaEstafetas([H|T], [(HC, NC)|TC], R) :- pertenceC(H, [(HC, NC)|TC]),1
%											%atualizaEstafeta(H, [(HC, NC)|TC], A).
%											NC is N + 1, 
											contaEstafetas(T, A, R). 

%contaEstafetas([H|T], [(HC, NC)|TC], R) :- nao(pertenceC(H, [(HC, NC)|TC])),
%											contaEstafetas(T, [(H, 1),(HC, NC)|TC], R).


%contaEstafetas([], [(HC, NC)|TC], R) :- maiorEstafeta((HC, NC), TC, R).


%atualizaEstafeta(ana, [(andre, 1), (bruno, 2), (ana, 4), (maria, 1)], R)

%atualizaEstafeta(H, [], []).
%atualizaEstafeta(H, [(H, NC)|TC], [(H, N)|A]) :- N is NC + 1,
%												atualizaEstafeta(H, TC, A).
%atualizaEstafeta(H, [(HC, NC)|TC], [(HC, NC)|A]) :- H \= HC, 
%										atualizaEstafeta(H, TC, A).




%maiorEstafeta((H, N), [], H).
%maiorEstafeta((H, N), [(H2, N2)|T], R) :- N2 > N,
%									maiorEstafeta((H2, N2), T, R).

%maiorEstafeta((H, N), [(H2, N2)|T], R) :- maiorEstafeta((H, N), T, R).









% 2) identificar que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente;

% Extensao do predicado estafetasEntregasCliente: Cliente, Lista encomendas, Lista de estafetas -> {V,F}


% -------------- se consideramos apenas um tipo de encomenda -------------------------- 


estafetasEntregaClienteValida(C, T, E) :- entrega(E, _, T, _, _, _, C, _, _, _, _).


estafetasEntregaCliente(C, T, R) :- findall(E, estafetasEntregaClienteValida(C, T, E), L),
									retiraDup(L, [], R).

todasEntregasDup(C, [], []).

todasEntregasDup(C, [T|Tail], R) :- estafetasEntregaCliente(C, T, Temp),
								concatenar(Temp, Temp1, R),
								todasEntregasDup(C, Tail, Temp1).

%executar esta
todasEntregas(C, T, R) :- todasEntregasDup(C, T, D),
						retiraDup(D, [], R), !.

retiraDup([], [], nenhum).
retiraDup([H|T], [], R) :- retiraDup(T, [H], R), !.

retiraDup([H|T], A, R) :- pertence(H, A),
						retiraDup(T, A, R), !.

retiraDup([H|T], [HA|TA], R) :- retiraDup(T, [H, HA|TA], R), !.

retiraDup([], L, L).








% 3) identificar os clientes servidos por um determinado estafeta

% Extensao do predicado clientesServidosEstafeta: Estafeta, Lista Clientes -> {V,F}


clientesServidosEstafeta(E, Clientes) :- findall(C, entrega(E, _, _, _, _, _, C, _, _, _, _), L),
										retiraDup(L, [], Clientes), !.




% 4) Extensao do predicado

















% 5) Extensao do predicado
























% 6) Extensao do predicado

















% identificar o número total de entregas pelos diferentes meios de transporte, num determinado intervalo de tempo;
% 7) Extensao do predicado

%veiculos(R) :- findall(V, entrega(,V,_,_,_,_,_,_,_,_), L), retiraDup(L, [], R).

%totalDifEntrega(Data1,Data2,Nentregas):- data(Data1),data(Data2),checkPeriodo(Data1,Data2),veiculos(Veiculos),calculaDifEntrega(Data1,Data2,[],Veiculos, Nentregas).
 
calculaDifEntrega(Data1,Data2,N,[Vult],Nentregas):- Nentregas= [(Vult,X)|N],calculaVentrega(Vult,X,Data1,Data2).

calculaDifEntrega(Data1,Data2, N , [Vatual,Vprox|Outros] , Nentregas) :- Novo = [(Vatual,X)|N],
						                         						 calculaVentrega(Vatual,X,Data1,Data2),
                                                                         calculaDifEntrega(Data1,Data2,Novo,[Vprox|Outros],Nentregas).

%calculaVentrega(Veiculo,N,D1,D2):- findall(_,(entrega(_,Veiculo,_,_,_,_,_,_,_,D),checkData(D1,D2,D)),L), length(L,N).










%identificar o número total de entregas pelos estafetas, num determinado intervalo de tempo;
% 8) Extensao do predicado



%estafetas(R) :- findall(E, entrega(E,_,_,_,_,_,_,_,_,_), L), retiraDup(L, [], R).

%totalEntregasEstafetas(Data1,Data2,Nentregas):- data(Data1),data(Data2), checkPeriodo(Data1,Data2),estafetas(Estafetas), calculaEntregas(Data1,Data2,[[]],Estafetas, Nentregas).
 
%calculaEntregas(Data1,Data2,N,[Eult],Nentregas):-  
%											Nentregas = [[Vult,X]|N],
%											calculaEentrega(Vult,X,Data1,Data2).
											 
%calculaEntregas(Data1,Data2, N , [Eatual,Eprox|Outros] , Nentregas) :- 
%																		  N = [[Eatual,X]|N],
%																		  calculaEentrega(Eatual,X,Data1,Data2),
%																		  calculaEntregas(Data1,Data2,N,[Eprox|Outros],Nentregas).

%calculaEentrega(E,N,D1,D2):- findall(_,(entrega(E,_,_,_,_,_,_,_,_,D),checkData(D1,D2,D)),L), length(L,N).













% 9)  calcular o número de encomendas entregues e não entregues pela Green Distribution, num determinado período de tempo;



verificaPeriodo(D1, D2, (P, DEnc, D/M/A/H)) :- entrega((_,_,_,P,_,_,_,_,_, DEnc, D/M/A/H)),
												checkData(D1, D2, D/M/A/H).


numEncomendas(D1, D2, Ent, NEnt) :- 












% 10) calcular o peso total transportado por estafeta num determinado dia


pesoTransEstafeta(D/M/A/_, (E, P)) :- entrega(E,_,_,P,_,_,_,_,_, _, D/M/A/_).
									
pesoTransEstafetaDia(D/M/A/_, R) :- findall((E, P), pesoTransEstafeta(D/M/A/_, (E, P)), L),
									agrupa(L, [], R).



agrupa([], R, R).
agrupa([H|T], [], R) :- agrupa(T, [H], R), !.

agrupa([H1|T1], [H2|T2], R) :- pertenceC(H1, [H2|T2]),
								atualizaPesos(H1, [H2|T2], A),
								agrupa(T1, A, R), !.

agrupa([H1|T1], [H2|T2], R) :- agrupa(T1, [H1, H2|T2], R), !.




atualizaPesos(X, [], []).
atualizaPesos((Nome1, Peso1), [(Nome1, Peso2)|T2], [(Nome1, PesoTotal)|A]) :-  PesoTotal is Peso1 + Peso2,
																				atualizaPesos((Nome1, Peso1), T2, A), !.

atualizaPesos((Nome1, Peso1), [(Nome2, Peso2)|T2], [(Nome2, Peso2)|A]) :- Nome1 \= Nome2,
																			atualizaPesos((Nome1, Peso1), T2, A), !.





estafetas(R) :- findall(E, entrega(E,_,_,_,_,_,_,_,_,_), L),
				retiraDup(L, [], R).


%somaValores(E, [], 0).
%somaValores(E, [H|T], (E, R)) :- somaValores(E, T, R1),
%						R is H + R1.




pertenceC( (X, P),[(X, N)|L] ).
pertenceC( (X, P),[(Y, N)|L] ) :-
    X \= Y,
    pertenceC( (X, P),L ).










%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado concatenar: Lista1,Lista2,Resultado -> {V,F}

concatenar(L1, [], L1).
concatenar([], L2, L2).
concatenar([H1|T1], [H2|T2], [H1|L]) :-
	concatenar(T1, [H2|T2], L).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado pertence: Elemento,Lista -> {V,F}

pertence( X,[X|L] ).
pertence( X,[Y|L] ) :-
    X \= Y,
    pertence( X,L ).


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
quantosIguais([H|T], N) :- pertence(H,T),
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
