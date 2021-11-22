%----------------------------------------------------------------------------------------------
%--------------------- Inteligência Artificial -> Parte I -----------------------------
%----------------------------------------------------------------------------------------------

%----------------Elementos do Grupo -----------------------------
%------- Ricardo Miguel Santos Gomes, A93785
%-------
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




% Extensao do predicado entrega: estafeta, veiculo, tipoEncomenda, pesoEnc, volEnc?, prazo, velocidade, cliente, rua, classificacao, data.


entrega(manuel, bicicleta, comida, 2, 1, 10, maria, 'avenida da liberdade, braga', 4, 11/10/2021).
entrega(manuel, bicicleta, comida, 2, 1, 10, maria, 'avenida da liberdade, braga', 4, 11/10/2021).
entrega(manuel, bicicleta, comida, 2, 1, 10, maria, 'avenida da liberdade, braga', 4, 11/10/2021).








entrega(manuel, bicicleta, comida, 2, 1, 10, maria, 'avenida da liberdade, braga', 4, 11/10/2021/12).
entrega(jose, bicicleta, comida, 4, 1, 10, ana, 'rua direita, barcelos', 5, 11/10/2021/12).
entrega(fabio, mota, roupa, 12, 24, 35, filipa, 'rua de campelo, guimaraes', 5, 11/10/2021/17).
entrega(marco, carro, movel, 80, 48, 25, cristina, 'travessa da igreja, famalicao', 3, 11/10/2021/17).

entrega(jose, bicicleta, comida, 4, 1, 10, ana, 'rua direita, barcelos', 5, 11/10/2021/20).
entrega(manuel, bicicleta, comida, 1, 1, 10, ana, 'rua direita, barcelos', 5, 11/10/2021/19).
entrega(fabio, bicicleta, comida, 2, 1, 10, ana, 'rua direita, barcelos', 3, 12/10/2021/21).


entrega(manuel, bicicleta, comida, 1, 1, 10, ana, 'rua direita, barcelos', 5, 11/10/2021/13).
entrega(manuel, mota, roupa, 10, 18, 35, maria, 'avenida da liberdade, braga', 3, 12/10/2021/11).
entrega(manuel, mota, comida, 3, 0.5, 35, cristina, 'travessa da igreja, famalicao', 4, 12/10/2021/14).
entrega(manuel, carro, movel, 74, 30, 25, filipa, 'rua de campelo, guimaraes', 5, 13/10/2021/14).
entrega(manuel, carro, comida, 3, 0.5, 25, cristina, 'travessa da igreja, famalicao', 4, 15/10/2021/18).

entrega(jose, carro, roupa, 17, 4, 25, ana, 'rua direita, barcelos', 2, 15/10/2021/18).
entrega(jose, mota, comida, 2, 1, 35, maria, 'avenida da liberdade, braga', 3, 14/10/2021/11).
entrega(jose, mota, comida, 3, 0.5, 35, filipa, 'rua de campelo, guimaraes', 1, 12/10/2021/10).
entrega(jose, carro, movel, 74, 30, 25, filipa, 'rua de campelo, guimaraes', 5, 13/10/2021/9).
entrega(jose, carro, movel, 60, 5, 25, ana, 'rua direita, barcelos', 3, 14/10/2021/15).

entrega(fabio, carro, movel, 45, 17, 25, ana, 'rua direita, barcelos', 2, 15/10/2021/15).
entrega(fabio, mota, roupa, 9, 5, 35, maria, 'avenida da liberdade, braga', 3, 12/10/2021/16).
entrega(fabio, mota, comida, 4, 2, 35, filipa, 'rua de campelo, guimaraes', 1, 12/10/2021/21).
entrega(fabio, carro, movel, 53, 25, 25, cristina, 'travessa da igreja, famalicao', 2, 13/10/2021/4).
entrega(fabio, bicicleta, comida, 2, 1, 10, ana, 'rua direita, barcelos', 3, 12/10/2021/13).

entrega(marco, bicicleta, roupa, 3, 1, 10, ana, 'rua direita, barcelos', 2, 14/10/202/9).
entrega(marco, mota, comida, 4, 0.5, 35, maria, 'avenida da liberdade, braga', 4, 12/10/2021/10).
entrega(marco, mota, comida, 3, 0.5, 35, cristina, 'travessa da igreja, famalicao', 5, 12/10/2021/11).
entrega(marco, carro, movel, 74, 30, 25, filipa, 'rua de campelo, guimaraes', 5, 13/10/2021/11).
entrega(marco, carro, movel, 89, 23, 25, cristina, 'travessa da igreja, famalicao', 5, 15/10/2021/14).


%entrega(E, V, T, P, Pr, Vel, C, R, Cla, D) :-
%	veiculo(V, P, Vel),
%	classificacao(Cla),
%	morada(C, R).
%	data(D).

% Extensao do predicado veiculo: veiculo, peso, velocidade -> {V,F}
veiculo(bicicleta, P, V) :-
	integer(P) =< 5, 
	V =:= 10.
veiculo(mota, P, V) :-
	P =< 20,
	V =:= 35.
veiculo(carro, P, V) :-
	P =< 100,
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


estafetaMaisVezesTransp(V, R) :- findall(E, entrega(E, V, _, _, _, _, _, _, _, _), L),
								contaEstafetas(L, [], R).

%contaEstafetas([], [], nenhum). %talvez não seja necessário

%contaEstafetas([H|T], [], R) :- contaEstafetas(T, [(H, 1)], R).

contaEstafetas([H|T], [(HC, NC)|TC], R) :- pertenceC(H, [(HC, NC)|TC]),
											atualizaEstafeta(H, [(HC, NC)|TC], A).
											contaEstafetas(T, A, R). 

contaEstafetas([H|T], [(HC, NC)|TC], R) :- nao(pertenceC(H, [(HC, NC)|TC])),
											contaEstafetas(T, [(H, 1),(HC, NC)|TC], R).


contaEstafetas([], [(HC, NC)|TC], R) :- maiorEstafeta((HC, NC), TC, R).


% atualizaEstafeta(ana, [(andre, 1), (bruno, 2), (ana, 4), (maria, 1)], R)

atualizaEstafeta(H, [], []).
atualizaEstafeta(H, [(H, NC)|TC], [(H, N)|A]) :- N is NC + 1,
												atualizaEstafeta(H, TC, A).
atualizaEstafeta(H, [(HC, NC)|TC], [(HC, NC)|A]) :- H \= HC, 
										atualizaEstafeta(H, TC, A).




maiorEstafeta((H, N), [], H).
maiorEstafeta((H, N), [(H2, N2)|T], R) :- N2 > N,
									maiorEstafeta((H2, N2), T, R).

maiorEstafeta((H, N), [(H2, N2)|T], R) :- maiorEstafeta((H, N), T, R).

pertenceC( X,[(X, N)|L] ).
pertenceC( X,[(Y, N)|L] ) :-
    X \= Y,
    pertenceC( X,L ).







% 2) identificar que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente;

% Extensao do predicado estafetasEntregasCliente: Cliente, Lista encomendas, Lista de estafetas -> {V,F}


% -------------- se consideramos apenas um tipo de encomenda -------------------------- 


estafetasEntregaClienteValida(C, T, E) :- entrega(E, _, T, _, _, _, C, _, _, _).


estafetasEntregaCliente(C, T, R) :- findall(E, estafetasEntregaClienteValida(C, T, E), L),
									retiraDup(L, [], R).

todasEntregasDup(C, [], []).

todasEntregasDup(C, [T|Tail], R) :- estafetasEntregaCliente(C, T, Temp),
								concatenar(Temp, Temp1, R),
								todasEntregasDup(C, Tail, Temp1).

%executar esta
todasEntregas(C, T, R) :- todasEntregasDup(C, T, D),
						retiraDup(D, [], R).

retiraDup([], [], nenhum).
retiraDup([H|T], [], R) :- retiraDup(T, [H], R).

retiraDup([H|T], A, R) :- pertence(H, A),
						retiraDup(T, A, R).

retiraDup([H|T], [HA|TA], R) :- retiraDup(T, [H, HA|TA], R).

retiraDup([], L, L).








% 3) identificar os clientes servidos por um determinado estafeta

% Extensao do predicado clientesServidosEstafeta: Estafeta, Lista Clientes -> {V,F}


clientesServidosEstafeta(E, Clientes) :- findall(C, entrega(E, _, _, _, _, _, C, _, _, _), L),
										retiraDup(L, [], Clientes).




% 4) Extensao do predicado

















% 5) Extensao do predicado
























% 6) Extensao do predicado

















% identificar o número total de entregas pelos diferentes meios de transporte, num determinado intervalo de tempo;
% 7) Extensao do predicado

%estafetas= [manuel, fabio, marco,jose]

%totalDifEntrega(Data1,Data2,N):- calculaDifEntrega(Data1,Data2,N, estafetas).
 
%calculaDifEntrega(Data1,Data2, N , Estafetas) :- findall(X,(entrega(E, _ , _  _ , _ , _ , _ , _ , _ , D), D => Data1 , D =< Data2),L) , len(L,N).

















% 8) Extensao do predicado















% 9) Extensao do predicado















% 10) Extensao do predicado











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