%-------------------------------------------------------------------------
%---------------        Base de conhecimento         	------------------
%-------------------------------------------------------------------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% estado inicial
inicial(greenDistribution).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% estado final
final(inicial(greenDistribution)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%Extensão do predicado objetivo: Localidade -> {V,F}
objetivo(greenDistribution).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado move: LocalidadeOigem, LocalidadeDestino, CustoDistancia, CustoTempo -> {V,F}
%move(fraiao,maximinos,2,3).
%move(fraiao,celeiros,6,5).
move(fraiao,palmeira,4,3).
move(maximinos,celeiros,1,1).
move(celeiros,gondizalves,1,3).
move(palmeira,gondizalves,9,7).
move(gondizalves,greenDistribution,2,9).
%move(adaufe,mireDeTibaes,5,2).
move(adaufe,gualtar,9,3).
%move(adaufe,greenDistribution,5,5).
move(mireDeTibaes,saoVitor,2,3).
move(gualtar,saoVitor,6,4).
move(gualtar,greenDistribution,1,9).
move(saoVitor,semelhe,3,1).
move(semelhe,trandeiras,8,7).
%move(trandeiras,real,2,3).
move(real,saoVicente,5,9).
move(real,greenDistribution,8,2).
move(saoVicente,pedralva,6,2).
%move(saoVicente,tenoes,4,1).
move(pedralva,priscos,7,4).
move(tenoes,cividade,2,8).
%move(tenoes,priscos,1,1).
move(priscos,padimDaGraca,4,8).
move(cividade,crespos,9,4).
move(cividade,greenDistribution,3,5).
move(padimDaGraca,crespos,2,4).
%move(padimDaGraca,ferreiros,3,2).
move(crespos,tadim,7,3).
%move(ferreiros,tadim,5,5).
%move(ferreiros,celeirosDup,1,9).
%move(tadim,nogueira,3,7).
move(tadim,greenDistribution,4,9).
%move(celeirosDup,nogueira,1,3).

%adjacente(Origem,Destino,D,T) :- adjacente(Destino,Origem,D,T).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado estima: Localidade, EstimaDistancia, EstimaTempo -> {V,F}

%estima(elvas,270, 150).
%estima(lisboa,0, 0).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado entrega: estafeta, veiculo, distancia, tipoEncomenda, pesoEnc, prazo, velocidade, cliente, rua, classificacao, dataEncomenda, dataEntrega -> {V, F}
entrega(manuel, bicicleta, 12, comida, 2, 1, 10, maria, 'crespos', 4, 11/10/2021/10, 11/10/2021/11).
entrega(manuel, bicicleta, 12, comida, 2, 1, 10, maria, 'crespos', 4, 11/10/2021/13, 11/10/2021/14).
entrega(manuel, bicicleta, 12, comida, 2, 1, 10, maria, 'crespos', 4, 11/10/2021/16, 11/10/2021/18).
entrega(manuel, bicicleta, 12, comida, 2, 1, 10, maria, 'crespos', 4, 11/10/2021/12, 11/10/2021/13).
entrega(jose, bicicleta, 15, comida, 4, 1, 10, ana, 'nogueira', 5, 11/10/2021/12, 11/10/2021/13).
entrega(fabio, mota, 20, roupa, 12, 24, 35, filipa, 'tadim', 5, 12/10/2021/13, 12/10/2021/19).
entrega(marco, carro, 23, movel, 80, 48, 25, cristina, 'adaufe', 3, 11/10/2021/17, 14/10/2021/10).
entrega(jose, bicicleta, 15, comida, 4, 1, 10, ana, 'nogueira', 5, 11/10/2021/20, 11/10/2021/21).
entrega(manuel, bicicleta, 15, comida, 1, 1, 10, ana, 'nogueira', 5, 11/10/2021/19, 11/10/2021/20).
entrega(fabio, bicicleta, 15, comida, 2, 1, 10, ana, 'nogueira', 3, 12/10/2021/21, 12/10/2021/22).
entrega(manuel, bicicleta, 15, comida, 1, 1, 10, ana, 'nogueira', 5, 11/10/2021/13, 11/10/2021/15).
entrega(manuel, mota, 12, roupa, 10, 18, 35, maria, 'crespos', 3, 12/10/2021/11, 13/10/2021/10).
entrega(manuel, mota, 23, comida, 3, 0.5, 35, cristina, 'adaufe', 4, 12/10/2021/14, 12/10/2021/15).
entrega(manuel, carro, 20, movel, 74, 30, 25, filipa, 'tadim', 5, 13/10/2021/14, 16/10/2021/10).
entrega(manuel, carro, 23, comida, 3, 0.5, 25, cristina, 'adaufe', 4, 15/10/2021/18, 15/10/2021/19).
entrega(jose, carro, 15, roupa, 17, 4, 25, ana, 'nogueira', 2, 15/10/2021/18, 17/10/2021/10).
entrega(jose, mota, 12, comida, 2, 1, 35, maria, 'crespos', 3, 14/10/2021/11, 14/10/2021/12).
entrega(jose, mota, 20, comida, 3, 0.5, 35, filipa, 'tadim', 1, 12/10/2021/10, 12/10/2021/12).
entrega(jose, carro, 20, movel, 74, 30, 25, filipa, 'tadim', 5, 13/10/2021/9, 16/10/2021/10).
entrega(jose, carro, 15, movel, 60, 5, 25, ana, 'nogueira', 3, 14/10/2021/15, 15/10/2021/10).
entrega(fabio, carro, 15, movel, 45, 17, 25, ana, 'nogueira', 2, 15/10/2021/15, 18/10/2021/10).
entrega(fabio, mota, 12, roupa, 9, 5, 35, maria, 'crespos', 3, 12/10/2021/16, 14/10/2021/10).
entrega(fabio, mota, 20, comida, 4, 2, 35, filipa, 'tadim', 1, 12/10/2021/21, 12/10/2021/22).
entrega(fabio, carro, 23, movel, 53, 25, 25, cristina, 'adaufe', 2, 13/10/2021/4, 15/10/2021/10).
entrega(fabio, bicicleta, 15, comida, 2, 1, 10, ana, 'nogueira', 3, 12/10/2021/13, 12/10/2021/14).
entrega(marco, bicicleta, 15, roupa, 3, 1, 10, ana, 'nogueira', 2, 14/10/2021/9, 14/10/2021/18).
entrega(marco, mota, 12, comida, 4, 0.5, 35, maria, 'crespos', 4, 12/10/2021/10, 12/10/2021/12).
entrega(marco, mota, 23, comida, 3, 0.5, 35, cristina, 'adaufe', 5, 12/10/2021/11, 12/10/2021/12).
entrega(marco, carro, 20, movel, 74, 30, 25, filipa, 'tadim', 5, 13/10/2021/11, 16/10/2021/10).
entrega(marco, carro, 23, movel, 89, 23, 25, cristina, 'adaufe', 5, 15/10/2021/14, 19/10/2021/10).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado morada: cliente, freguesia -> {V, F}
morada(maria, 'crespos').
morada(ana, 'nogueira').
morada(filipa, 'tadim').
morada(cristina, 'adaufe').

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado calcula_tempo: cliente, freguesia -> {V, F}
calcula_tempo(bicicleta,Distancia,PesoEnc, Tempo) :- VelMed is (10 - (0.7 * PesoEnc)), Tempo is Distancia / VelMed. 
calcula_tempo(mota,Distancia,PesoEnc, Tempo) :- VelMed is (35 - (0.5 * PesoEnc)), Tempo is Distancia / VelMed. 
calcula_tempo(carro,Distancia,PesoEnc, Tempo) :- VelMed is (25 - (0.1 * PesoEnc)), Tempo is Distancia / VelMed. 


%----------------------------------------------------------------------------------------------------------------------------------------------
%----------------------------------------------------------- Distancia ------------------------------------------------------------------------
%----------------------------------------------------------------------------------------------------------------------------------------------

%----------------------- Profundidade ---------------------------------------------------------------------------------------------------------

%profundidade(Nodo, [Nodo|Caminho], C) :- profundidadeprimeiro(Nodo, [Nodo], Caminho, C).
%
%
%profundidadeprimeiro(Nodo,_, [], 0) :- objetivo(Nodo).
%
%profundidadeprimeiro(Nodo, Historico, [ProxNodo|Caminho], C) :- adjacente(Nodo, ProxNodo, C1),
%    															nao(membro(ProxNodo, Historico)),
%																profundidadeprimeiro(ProxNodo, [ProxNodo|Historico], Caminho, C2), 
%																C is C1 + C2.	




profundidade(NodoObjetivo, Caminho, C) :- inicial(Inicio),
										profundidadeprimeiroInicial(NodoObjetivo, Inicio, [Inicio], Caminho, C).

%coloca a greenDistribution no inicio
profundidadeprimeiroInicial(NodoObjetivo, Inicio, Historico, [Inicio, ProxNodo|Caminho], C) :- adjacente(Inicio, ProxNodo, C1),
																								nao(membro(ProxNodo, Historico)),
																								profundidadeprimeiro(NodoObjetivo, ProxNodo, [ProxNodo|Historico], Caminho, C2), 
																								C is C1 + C2.	

profundidadeprimeiro(NodoObjetivo, NodoAtual, Historico, [NodoObjetivo], C1) :- adjacente(NodoAtual, NodoObjetivo, C1), !.

profundidadeprimeiro(NodoObjetivo, NodoAtual, Historico, [ProxNodo|Caminho], C) :- adjacente(NodoAtual, ProxNodo, C1),
    																				nao(membro(ProxNodo, Historico)),
																					profundidadeprimeiro(NodoObjetivo, ProxNodo, [ProxNodo|Historico], Caminho, C2), 
																					C is C1 + C2.	



adjacente(Nodo, ProxNodo, C) :- move(Nodo, ProxNodo, C, _).
adjacente(Nodo, ProxNodo, C) :- move(ProxNodo, Nodo, C, _).



%executar esta
melhorProfundidade(NodoObjetivo, Caminho, Custo) :- findall((SS, CC), profundidade(NodoObjetivo, SS, CC), L), 
													minimo(L, (Caminho, Custo)), !. 


minimo([(P,X)],(P,X)).
minimo([(Px,X)|L],(Py,Y)):- minimo(L,(Py,Y)), X>Y. 
minimo([(Px,X)|L],(Px,X)):- minimo(L,(Py,Y)), X=<Y.

%>



%----------------------- Profundidade Com Limite ---------------------------------------------------------------------------------------------------------

profundidadeLimite(NodoObjetivo, Limite, Caminho, C) :- inicial(Inicio),
														profundidadeprimeiroLimiteInicial(NodoObjetivo, Limite, Inicio, [Inicio], Caminho, C).



%coloca a greenDistribution no inicio
profundidadeprimeiroLimiteInicial(NodoObjetivo, Limite, Inicio, Historico, [Inicio, ProxNodo|Caminho], C) :- adjacente(Inicio, ProxNodo, C1),
																								nao(membro(ProxNodo, Historico)),
																								length([Inicio, ProxNodo|Caminho], Tam),
    																							Tam - 1 < Limite + 1,
																								profundidadeprimeiroLimite(NodoObjetivo, Limite, ProxNodo, [ProxNodo|Historico], Caminho, C2), 
																								C is C1 + C2.	



profundidadeprimeiroLimite(NodoObjetivo, NodoAtual, Historico, [NodoObjetivo], C1) :- adjacente(NodoAtual, NodoObjetivo, C1), !.

profundidadeprimeiroLimite(NodoObjetivo, Limite, ProxNodo, Historico, [ProxNodo|Caminho], C) :- adjacente(Nodo, ProxNodo, C1),
    															nao(membro(ProxNodo, Historico)),
    															length([ProxNodo|Historico], Tam),
    															Tam - 1 < Limite + 1,  % é o mesmo que ter <=, o limite continua a ser respeitado
																profundidadeprimeiroLimite(NodoObjetivo, ProxNodo, Limite, [ProxNodo|Historico], Caminho, C2), 
																C is C1 + C2.	

%executar esta
melhorProfundidadeLimite(NodoObjetivo, Limite, Caminho, Custo) :- findall((SS, CC), profundidadeLimite(NodoObjetivo, Limite, SS, CC), L), 
							minimo(L, (CaminhoInverso, Custo)), 
							inverso(CaminhoInverso, Caminho).





%profundidadeLimite(Nodo, Limite, [Nodo|Caminho], C) :- profundidadeprimeiroLimite(Nodo, Limite, [Nodo], Caminho, C).
%
%
%profundidadeprimeiroLimite(Nodo, _, _, [], 0) :- objetivo(Nodo).
%
%profundidadeprimeiroLimite(Nodo, Limite, Historico, [ProxNodo|Caminho], C) :- adjacente(Nodo, ProxNodo, C1),
%   															nao(membro(ProxNodo, Historico)),
%    															length([ProxNodo|Historico], Tam),
%   															Tam - 1 < Limite + 1,  % é o mesmo que ter <=, o limite continua a ser respeitado
%																profundidadeprimeiroLimite(ProxNodo, Limite, [ProxNodo|Historico], Caminho, C2), 
%																C is C1 + C2.	


%>

%----------------------- Largura ---------------------------------------------------------------------------------------------------------

%largura(Nodo, [Nodo|Caminho], C) :- larguraprimeiro(Nodo, [Nodo], Caminho, C).
%
%
%larguraprimeiro(Nodo,_, [], 0) :- objetivo(Nodo).
%
%larguraprimeiro(Nodo, Historico, [ProxNodo|Caminho], C) :- adjacente(Nodo, ProxNodo, C1),
%    														nao(membro(ProxNodo, Historico)),
%															larguraprimeiro(ProxNodo, [ProxNodo|Historico], Caminho, C2), 
%															C is C1 + C2.	



%executar esta
%melhorLargura(Nodo, Caminho, Custo) :- findall((SS, CC), largura(Nodo, SS, CC), L), 
%							minimo(L, (CaminhoInverso, Custo)), 
%							inverso(CaminhoInverso, Caminho).


%>

%--------------------------------- predicados auxiliares

inverso(Xs, Ys):-
	inverso(Xs, [], Ys).

inverso([], Xs, Xs).
inverso([X|Xs],Ys, Zs):-
	inverso(Xs, [X|Ys], Zs).

seleciona(E, [E|Xs], Xs).
seleciona(E, [X|Xs], [X|Ys]) :- seleciona(E, Xs, Ys).

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

membro(X, [X|_]).
membro(X, [_|Xs]):-
	membro(X, Xs).		

escrever([]).
escrever([X|L]):- write(X), nl, escrever(L).