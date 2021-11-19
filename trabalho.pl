%----------------------------------------------------------------------------------------------
%--------------------- Inteligência Artificial -> Parte I -----------------------------
%----------------------------------------------------------------------------------------------
%
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
% dia:
%	11/10/2021
%	12/10/2021
%	13/10/2021
%	14/10/2021
%	15/10/2021
%
%

% Extensao do predicado entrega: estafeta, veiculo, tipoEncomenda, pesoEnc, volEnc?, prazo, velocidade, cliente, rua, classificacao, dia.
entrega(manuel, bicicleta, comida, 2, 1, 10, maria, 'avenida da liberdade, braga', 4, 11/10/2021).
entrega(jose, bicicleta, comida, 4, 1, 10, ana, 'rua direita, barcelos', 5, 11/10/2021).
entrega(fabio, mota, roupa, 12, 24, 35, filipa, 'rua de campelo, guimaraes', 5, 11/10/2021).
entrega(marco, carro, movel, 80, 48, 25, cristina, 'travessa da igreja, famalicao', 3, 11/10/2021).

entrega(jose, bicicleta, comida, 4, 1, 10, ana, 'rua direita, barcelos', 5, 11/10/2021).
entrega(manuel, bicicleta, comida, 1, 1, 10, ana, 'rua direita, barcelos', 5, 11/10/2021).
entrega(fabio, bicicleta, comida, 2, 1, 10, ana, 'rua direita, barcelos', 3, 12/10/2021).


entrega(manuel, bicicleta, comida, 1, 1, 10, ana, 'rua direita, barcelos', 5, 11/10/2021).
entrega(manuel, mota, roupa, 10, 18, 35, maria, 'avenida da liberdade, braga', 3, 12/10/2021).
entrega(manuel, mota, comida, 3, 0.5, 35, cristina, 'travessa da igreja, famalicao', 4, 12/10/2021).
entrega(manuel, carro, movel, 74, 30, 25, filipa, 'rua de campelo, guimaraes', 5, 13/10/2021).
entrega(manuel, carro, comida, 3, 0.5, 25, cristina, 'travessa da igreja, famalicao', 4, 15/10/2021).

entrega(jose, carro, roupa, 17, 4, 25, ana, 'rua direita, barcelos', 2, 15/10/2021).
entrega(jose, mota, comida, 2, 1, 35, maria, 'avenida da liberdade, braga', 3, 14/10/2021).
entrega(jose, mota, comida, 3, 0.5, 35, filipa, 'rua de campelo, guimaraes', 1, 12/10/2021).
entrega(jose, carro, movel, 74, 30, 25, filipa, 'rua de campelo, guimaraes', 5, 13/10/2021).
entrega(jose, carro, movel, 60, 5, 25, ana, 'rua direita, barcelos', 3, 14/10/2021).

entrega(fabio, carro, movel, 45, 17, 25, ana, 'rua direita, barcelos', 2, 15/10/2021).
entrega(fabio, mota, roupa, 9, 5, 35, maria, 'avenida da liberdade, braga', 3, 12/10/2021).
entrega(fabio, mota, comida, 4, 2, 35, filipa, 'rua de campelo, guimaraes', 1, 12/10/2021).
entrega(fabio, carro, movel, 53, 25, 25, cristina, 'travessa da igreja, famalicao', 2, 13/10/2021).
entrega(fabio, bicicleta, comida, 2, 1, 10, ana, 'rua direita, barcelos', 3, 12/10/2021).

entrega(marco, bicicleta, roupa, 3, 1, 10, ana, 'rua direita, barcelos', 2, 14/10/2021).
entrega(marco, mota, comida, 4, 0.5, 35, maria, 'avenida da liberdade, braga', 4, 12/10/2021).
entrega(marco, mota, comida, 3, 0.5, 35, cristina, 'travessa da igreja, famalicao', 5, 12/10/2021).
entrega(marco, carro, movel, 74, 30, 25, filipa, 'rua de campelo, guimaraes', 5, 13/10/2021).
entrega(marco, carro, movel, 89, 23, 25, cristina, 'travessa da igreja, famalicao', 5, 15/10/2021).





% Extensao do predicado veiculo: veiculo, peso, velocidade -> {V,F}
veiculo(bicicleta, P, V) :-
	P =< 5, 
	V == 10.
veiculo(mota, P, V) :-
	P =< 20,
	V == 35.
veiculo(carro, P, V) :-
	P =< 100,
	V == 25.


% Extensao do predicado classificacao: classificacao -> {V,F}

classificacao(C) :-
	pertence(C, [0,1,2,3,4,5]).


% Extensao do predicado morada: cliente, rua -> {V, F}

morada(maria, 'avenida da liberdade, braga').
morada(ana, 'rua direita, barcelos').
morada(filipa, 'rua de campelo, guimaraes').
morada(cristina, 'travessa da igreja, famalicao').





% 1) identificar o estafeta que utilizou mais vezes um meio de transporte mais ecológico;

% Extensao do predicado estafetaMaisVezesTransp: Veiculo, Estafeta -> {V,F}


estafetaMaisVezesTransp(V, R) :- findall(E, entrega(E, V, _, _, _, _, _, _, _, _), L),
								contaEstafetas(L, [], R).

%contaEstafetas([], [], nenhum). %talvez não seja necessário

contaEstafetas([H|T], [], R) :- contaEstafetas(T, [(H, 1)], R).

contaEstafetas([H|T], [(HC, NC)|TC], R) :- pertenceC(H, [(HC, N)|TC]),
											atualizaEstafeta(H, [(HC, N)|TC], R).
											%contaEstafetas(T, A, R). %%%%%%%%%%%%%

contaEstafetas([H|T], [(HC, NC)|TC], R) :- nao(pertenceC(H, [(HC, N)|TC])),
											contaEstafetas(T, [(H, 1),(HC, NC)|TC], R).


contaEstafetas([], [(HC, NC)|TC], R) :- maiorEstafeta((HC, NC), TC, R).


% atualizaEstafeta(ana, [(andre, 1), (bruno, 2), (ana, 4), (maria, 1)], R)

atualizaEstafeta(H, [], []).
atualizaEstafeta(H, [(H, NC)|TC], [(H, N)|TC]) :- N is NC + 1,
												atualizaEstafeta(H, TC, TC).
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







% 2) Extensao do predicado













% 3) Extensao do predicado


















% 4) Extensao do predicado

















% 5) Extensao do predicado
























% 6) Extensao do predicado


















% 7) Extensao do predicado



















% 8) Extensao do predicado















% 9) Extensao do predicado















% 10) Extensao do predicado















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