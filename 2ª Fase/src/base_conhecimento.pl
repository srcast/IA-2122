%-------------------------------------------------------------------------
%---------------        Base de conhecimento		------------------
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
move(fraiao,maximinos,2).
move(fraiao,celeiros,6).
move(fraiao,palmeira,4).
move(maximinos,celeiros,1).
move(celeiros,gondizalves,1).
move(palmeira,gondizalves,9).
move(gondizalves,greenDistribution,2).
move(adaufe,mireDeTibaes,5).
move(adaufe,gualtar,9).
move(adaufe,greenDistribution,5).
move(mireDeTibaes,saoVitor,2).
move(gualtar,saoVitor,6).
move(gualtar,greenDistribution,1).
move(saoVitor,semelhe,3).
move(semelhe,trandeiras,8).
move(trandeiras,real,2).
move(real,saoVicente,5).
move(real,greenDistribution,8).
move(saoVicente,pedralva,6).
move(saoVicente,tenoes,4).
move(pedralva,priscos,7).
move(tenoes,cividade,2).
move(tenoes,priscos,1).
move(priscos,padimDaGraca,4).
move(cividade,crespos,9).
move(cividade,greenDistribution,3).
move(padimDaGraca,crespos,2).
move(padimDaGraca,ferreiros,3).
move(crespos,tadim,7).
move(ferreiros,tadim,5).
move(ferreiros,espinho,1).
move(tadim,nogueira,3).
move(tadim,greenDistribution,4).
move(espinho,nogueira,1).

adjacente(Nodo, ProxNodo, C) :- move(Nodo, ProxNodo, C).
adjacente(Nodo, ProxNodo, C) :- move(ProxNodo, Nodo, C).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado estima: Localidade, EstimaDistancia -> {V,F}

estima(fraiao, 9).
estima(maximinos, 5).
estima(palmeira, 10).
estima(celeiros, 4).
estima(gondizalves, 2).
estima(adaufe, 4).
estima(mireDeTibaes, 9).
estima(saoVitor, 8).
estima(gualtar, 1).
estima(semelhe, 11).
estima(trandeiras, 9).
estima(real, 6).
estima(saoVicente, 12).
estima(pedralva, 14).
estima(priscos, 11).
estima(tenoes, 5).
estima(cividade, 2).
estima(crespos, 10).
estima(padimDaGraca, 11).
estima(ferreiros, 7).
estima(espinho, 7).
estima(nogueira, 6).
estima(tadim, 3).
estima(greenDistribution, 0).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado entrega: estafeta, tipoEncomenda, pesoEnc, prazo,cliente, rua, classificacao, dataEncomenda, dataEntrega -> {V, F}

% retirei veiculo, distancia e velocidade

entrega(manuel, comida, 2, 1, maria, 'crespos', 4, 11/10/2021/10, 11/10/2021/11).
%entrega(manuel, comida, 2, 1, maria, 'crespos', 4, 11/10/2021/13, 11/10/2021/14).
%entrega(manuel, comida, 2, 1, maria, 'crespos', 4, 11/10/2021/16, 11/10/2021/18).
%entrega(manuel, comida, 2, 1, maria, 'crespos', 4, 11/10/2021/12, 11/10/2021/13).
entrega(jose, comida, 4, 1, ana, 'nogueira', 5, 11/10/2021/12, 11/10/2021/13).
entrega(fabio, roupa, 12, 24, filipa, 'tadim', 5, 12/10/2021/13, 12/10/2021/19).
entrega(marco, movel, 2, 48, cristina, 'adaufe', 3, 11/10/2021/17, 14/10/2021/10).
entrega(jose, comida, 4, 1, ana, 'nogueira', 5, 11/10/2021/20, 11/10/2021/21).
entrega(manuel, comida, 1, 1, ana, 'nogueira', 5, 11/10/2021/19, 11/10/2021/20).
entrega(fabio, comida, 2, 1, ana, 'nogueira', 3, 12/10/2021/21, 12/10/2021/22).
entrega(manuel, comida, 1, 1, ana, 'nogueira', 5, 11/10/2021/13, 11/10/2021/15).
entrega(manuel, roupa, 10, 18, maria, 'crespos', 3, 12/10/2021/11, 13/10/2021/10).
entrega(manuel, comida, 3, 0.5, cristina, 'adaufe', 4, 12/10/2021/14, 12/10/2021/15).
entrega(manuel, movel, 74, 30, filipa, 'tadim', 5, 13/10/2021/14, 16/10/2021/10).
entrega(manuel, comida, 3, 0.5, cristina, 'adaufe', 4, 15/10/2021/18, 15/10/2021/19).
entrega(jose, roupa, 17, 4, ana, 'nogueira', 2, 15/10/2021/18, 17/10/2021/10).
entrega(jose, comida, 2, 1, maria, 'crespos', 3, 14/10/2021/11, 14/10/2021/12).
entrega(jose, comida, 3, 0.5, filipa, 'tadim', 1, 12/10/2021/10, 12/10/2021/12).
entrega(jose, movel, 74, 30, filipa, 'tadim', 5, 13/10/2021/9, 16/10/2021/10).
entrega(jose, movel, 60, 5, ana, 'nogueira', 3, 14/10/2021/15, 15/10/2021/10).
entrega(fabio, movel, 45, 17, ana, 'nogueira', 2, 15/10/2021/15, 18/10/2021/10).
entrega(fabio, roupa, 9, 5, maria, 'crespos', 3, 12/10/2021/16, 14/10/2021/10).
entrega(fabio, comida, 4, 2, filipa, 'tadim', 1, 12/10/2021/21, 12/10/2021/22).
entrega(fabio, movel, 53, 25, cristina, 'adaufe', 2, 13/10/2021/4, 15/10/2021/10).
entrega(fabio, comida, 2, 1, ana, 'nogueira', 3, 12/10/2021/13, 12/10/2021/14).
entrega(marco, roupa, 3, 1, ana, 'nogueira', 2, 14/10/2021/9, 14/10/2021/18).
entrega(marco, comida, 4, 0.5, maria, 'crespos', 4, 12/10/2021/10, 12/10/2021/12).
entrega(marco, comida, 3, 0.5, cristina, 'adaufe', 5, 12/10/2021/11, 12/10/2021/12).
entrega(marco, movel, 74, 30, filipa, 'tadim', 5, 13/10/2021/11, 16/10/2021/10).
entrega(marco, movel, 89, 23, cristina, 'adaufe', 5, 15/10/2021/14, 19/10/2021/10).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado morada: cliente, freguesia -> {V, F}
morada(maria, 'crespos').
morada(ana, 'nogueira').
morada(filipa, 'tadim').
morada(cristina, 'adaufe').

%--------------------------------- Predicados relacionados ao veículo - - - - - - - - - -  -  -  -  -   -

% irá gerar => [(Veiculo,Tempo,Distancia),...]
geraVeiculos([],_,_,L,L):-!.
geraVeiculos([(Caminho,Distancia)|Caminhos], Peso, Prazo, Lista, Veiculos):- DistanciaIda is Distancia / 2, escolheVeiculo(Peso,DistanciaIda,Veiculo,Prazo,Tempo),
																geraVeiculos(Caminhos,Peso,Prazo,[(Tempo,Distancia,Caminho,Veiculo)|Lista], Veiculos).




getV(V, [(A1,A2,A3,V)| _] , (A1,A2,A3,V)):- !.
getV( V , [_|CS] , X ) :- getV(V,CS,X).

getMostEco(Lista,X):- member((_,_,_,bicicleta),Lista), getV(bicicleta,Lista,X),!.
getMostEco(Lista,X):- member((_,_,_,mota),Lista), getV(mota,Lista,X),!.
getMostEco(Lista,X):- member((_,_,_,carro),Lista), getV(carro,Lista,X).


% escolhe veículo mais ecológico repeitando as restrições e prazo de tempo
escolheVeiculo(Peso,Distancia,Veiculo,Prazo,Tempo):- Peso =< 5 , calcula_tempo(bicicleta, Distancia, Peso, Tempo) , Tempo =< Prazo , Veiculo = bicicleta, !.
escolheVeiculo(Peso,Distancia,Veiculo,Prazo,Tempo):- Peso =< 20 , calcula_tempo(mota, Distancia, Peso, Tempo) , Tempo =< Prazo, Veiculo = mota, !.
escolheVeiculo(Peso,Distancia,Veiculo,_,Tempo):- Peso =< 100 , calcula_tempo(carro, Distancia, Peso, Tempo) , Veiculo = carro.


% Extensão do predicado calcula_tempo: veiculo, distancia, peso , tempo -> {V, F}
calcula_tempo(Veiculo,Distancia,PesoEnc, Tempo) :-  velMed(Veiculo,Vel),desconto_velocidade(Veiculo,Vel,PesoEnc, VelDesconto), Tempo is (Distancia / VelDesconto) + (Distancia / Vel).

velMed(bicicleta, V):- V is 10.
velMed(mota, V):- V is 35.
velMed(carro, V):- V is 25.

desconto_velocidade(bicicleta, Vel, Peso, NewVel) :- NewVel is Vel - (0.7*Peso).
desconto_velocidade(mota, Vel, Peso, NewVel) :- NewVel is Vel - (0.5*Peso).
desconto_velocidade(carro, Vel, Peso, NewVel) :- NewVel is Vel - (0.1*Peso).




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
%															nao(membro(ProxNodo, Historico)),
%																profundidadeprimeiro(ProxNodo, [ProxNodo|Historico], Caminho, C2),
%																C is C1 + C2.



%executar esta
profundidade(NodoObjetivo, CaminhoTodo, C) :- inicial(Inicio),
										profundidadeprimeiroInicial(NodoObjetivo, Inicio, [Inicio], Caminho, C2), !,
										duplicaCaminho(Caminho, CaminhoTodo),
										C is C2 * 2.


profundidadeprimeiroInicial(NodoObjetivo, Inicio,_, [Inicio, NodoObjetivo], C) :- adjacente(Inicio, NodoObjetivo, C).

%coloca a greenDistribution no inicio
profundidadeprimeiroInicial(NodoObjetivo, Inicio, Historico, [Inicio, ProxNodo|Caminho], C) :- adjacente(Inicio, ProxNodo, C1),
																								nao(membro(ProxNodo, Historico)),
																								profundidadeprimeiro(NodoObjetivo, ProxNodo, [ProxNodo|Historico], Caminho, C2),
																								C is C1 + C2.

profundidadeprimeiro(NodoObjetivo, NodoAtual, _ , [NodoObjetivo], C1) :- adjacente(NodoAtual, NodoObjetivo, C1), !.

profundidadeprimeiro(NodoObjetivo, NodoAtual, Historico, [ProxNodo|Caminho], C) :- adjacente(NodoAtual, ProxNodo, C1),
																				nao(membro(ProxNodo, Historico)),
																					profundidadeprimeiro(NodoObjetivo, ProxNodo, [ProxNodo|Historico], Caminho, C2),
																					C is C1 + C2.







melhorProfundidade(NodoObjetivo, Caminho, Custo) :- findall((Caminhos, Custos), profundidade(NodoObjetivo, Caminhos, Custos), L),
													minimo(L, (Caminho, Custo)), !.







duplicaCaminho(Caminho, CaminhoTodo) :- duplicaCaminhoAuxiliar(Caminho, [], CaminhoInverso),
										append(Caminho, CaminhoInverso, CaminhoTodo).

% devolve o caminho reverso sem o nodo objetivo
duplicaCaminhoAuxiliar([], [_|T], T).
duplicaCaminhoAuxiliar([H|T], Aux, CaminhoInverso) :- duplicaCaminhoAuxiliar(T, [H|Aux], CaminhoInverso).


%----------------------- Profundidade Com Limite ---------------------------------------------------------------------------------------------------------

%executar esta
%executar esta para testar os diferentes limites
profundidadeLimite(NodoObjetivo, Limite, CaminhoTodo, C) :- inicial(Inicio),
														profundidadeprimeiroLimiteInicial(NodoObjetivo, Limite, Inicio, [Inicio], Caminho, C2), !,
														duplicaCaminho(Caminho, CaminhoTodo),
														C is C2 * 2.

profundidadeprimeiroLimiteInicial(NodoObjetivo,_, Inicio,_, [Inicio, NodoObjetivo], C) :- adjacente(Inicio, NodoObjetivo, C).

%coloca a greenDistribution no inicio
profundidadeprimeiroLimiteInicial(NodoObjetivo, Limite, Inicio, Historico, [Inicio, ProxNodo|Caminho], C) :- adjacente(Inicio, ProxNodo, C1),
																								nao(membro(ProxNodo, Historico)),
																								length([ProxNodo|Historico], Tam),
																							Tam - 1 < Limite,
																								profundidadeprimeiroLimite(NodoObjetivo, Limite, ProxNodo, [ProxNodo|Historico], Caminho, C2),
																								C is C1 + C2.



profundidadeprimeiroLimite(NodoObjetivo, _, NodoAtual, _, [NodoObjetivo], C1) :- adjacente(NodoAtual, NodoObjetivo, C1), !.

profundidadeprimeiroLimite(NodoObjetivo, Limite, NodoAtual, Historico, [ProxNodo|Caminho], C) :- adjacente(NodoAtual, ProxNodo, C1),
															nao(membro(ProxNodo, Historico)),
															length([ProxNodo|Historico], Tam),
															Tam - 1 < Limite,  % é o mesmo que ter <=, o limite continua a ser respeitado
																profundidadeprimeiroLimite(NodoObjetivo, Limite, ProxNodo, [ProxNodo|Historico], Caminho, C2),
																C is C1 + C2.




melhorProfundidadeLimite(NodoObjetivo, Limite, Caminho, Custo) :- findall((SS, CC), profundidadeLimite(NodoObjetivo, Limite, SS, CC), L),
							minimo(L, (Caminho, Custo)), !.


%----------------------- Largura ---------------------------------------------------------------------------------------------------------




melhorLargura(NodoObjetivo, Caminho, Custo) :- findall((Caminhos, Custos), largura(NodoObjetivo, Caminhos, Custos), L),
													minimo(L, (Caminho, Custo)), !.


largura(Dest, Caminho, Custos):- inicial(Orig),
							largura2(Dest,[[Orig/0]],Cam), !,
							somaCustos(Cam, Custos2),
							retiraCustos(Cam, [NodoRetirar|Resto]),
							inverso([NodoRetirar|Resto], CaminhoIncompleto),
							append(CaminhoIncompleto, Resto, Caminho), % duplica o caminho sem o NodoObjetivo duplicado
							Custos is Custos2 * 2.

largura2(Dest,[[Dest/Cus|T]|_],[Dest/Cus|T]).

largura2(Dest,[LA|Outros],Cam):- LA=[Act|_],
							Act = Atual/Cus,
							findall([X/C|LA], (Dest\==Atual,adjacente(Atual,X, C),\+membroCustos(X,LA)),Novos),
							append(Outros,Novos,Todos),
							largura2(Dest,Todos,Cam).


somaCustos([], 0).
somaCustos([_/Custo|Resto], Custos) :- somaCustos(Resto, Custos2),
												Custos is Custo + Custos2.

retiraCustos([], []).
retiraCustos([Localidade/_|Resto], [Localidade|Outras]) :- retiraCustos(Resto, Outras).


membroCustos(Local, [Local/Cus|Resto]).
membroCustos(Local, [Nodo/Cus|Resto]) :- membroCustos(Local, Resto).








%----------------------- Gulosa ---------------------------------------------------------------------------------------------------------

resolve_gulosa_distancia(Nodo, CaminhoDistancia, CustoDist) :- estima(Nodo, EstimaD),
															agulosa_distancia([[Nodo]/0/EstimaD], [GD|T]/CustoDist2/_), !,
															inverso([GD|T], [NodoRetirar|CaminhoInverso]),
															append([GD|T], CaminhoInverso, CaminhoDistancia), %junta sem o NodoObjetivo duplicado
															CustoDist is CustoDist2 * 2.

agulosa_distancia(Caminhos, Caminho) :- obtem_melhor_distancia(Caminhos, Caminho),
										Caminho = [Nodo|_]/_/_,
										inicial(Nodo).

agulosa_distancia(Caminhos, SolucaoCaminho) :- obtem_melhor_distancia(Caminhos, MelhorCaminho),
												seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
												expande_agulosa_distancia(MelhorCaminho, ExpCaminhos),
												append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
											agulosa_distancia(NovoCaminhos, SolucaoCaminho).

obtem_melhor_distancia([Caminho], Caminho) :- !.
obtem_melhor_distancia([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos], MelhorCaminho) :- Est1 =< Est2, !,                                 %>
																		obtem_melhor_distancia([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho).

obtem_melhor_distancia([_|Caminhos], MelhorCaminho) :- obtem_melhor_distancia(Caminhos, MelhorCaminho).


expande_agulosa_distancia(Caminho, ExpCaminhos) :- findall(NovoCaminho, adjacente_distancia(Caminho,NovoCaminho), ExpCaminhos).


adjacente_distancia([Nodo|Caminho]/Custo/_, [ProxNodo,Nodo|Caminho]/NovoCusto/EstDist) :- move(Nodo, ProxNodo, PassoCustoDist),
																						\+ member(ProxNodo, Caminho),
																						NovoCusto is Custo + PassoCustoDist,
																						estima(ProxNodo, EstDist).



%-----------------------  A* ---------------------------------------------------------------------------------------------------------

resolve_aestrela(Nodo, Caminho, Custo) :- estima(Nodo, Estima),
										aestrela([[Nodo]/0/Estima], [GD|T]/Custo2/_), !,
										inverso([GD|T], [NodoRetirar|CaminhoInverso]),
										append([GD|T], CaminhoInverso, Caminho), %junta sem o NodoObjetivo duplicado
										Custo is Custo2 * 2.

aestrela(Caminhos, Caminho) :- obtem_melhor(Caminhos, Caminho),
								Caminho = [Nodo|_]/_/_,
								inicial(Nodo).

aestrela(Caminhos, SolucaoCaminho) :- obtem_melhor(Caminhos, MelhorCaminho),
									seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
									expande_aestrela(MelhorCaminho, ExpCaminhos),
									append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
									aestrela(NovoCaminhos, SolucaoCaminho).

obtem_melhor([Caminho], Caminho) :- !.

obtem_melhor([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos], MelhorCaminho) :- Custo1 + Est1 =< Custo2 + Est2, !,     %>
																			obtem_melhor([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho).

obtem_melhor([_|Caminhos], MelhorCaminho) :- obtem_melhor(Caminhos, MelhorCaminho).


expande_aestrela(Caminho, ExpCaminhos) :- findall(NovoCaminho, adjacente_distancia(Caminho,NovoCaminho), ExpCaminhos).















%--------------------------------------------------------------------------------------------------------------------------------
% Gerar os circuitos de entrega, caso existam, que cubram um determinado território (e.g. rua ou freguesia);

%gera um circuito para um determinado local
geraCircuito(NodoObjetivo, Caminho, C) :- inicial(Inicio),
										profundidadeprimeiroInicial(NodoObjetivo, Inicio, [Inicio], [H|T], C2),
										inverso([H|T], [NodoRetirar|Resto]),
										append([H|T], Resto, Caminho),
										C is C2 * 2.

% gera todos od circuitos para um determinado local
geraCircuitosObjetivo(NodoObjetivo, Caminhos) :- findall(Caminho,geraCircuito(NodoObjetivo, Caminho, C), Caminhos).






geraCircuitosComCustos(NodoObjetivo, Caminhos) :- findall((Caminho,C),geraCircuito(NodoObjetivo, Caminho, C), Caminhos).

%--------------------------------------------------------------------------------------------------------------------------------
% Identificar quais os circuitos com maior número de entregas (por volume e peso);


%identificar circuitos com um minimo de Peso
identificarCircuitos(Peso, Circuitos) :- geraCircuitosObjetivo(NodoObjetivo, Caminhos),
										calcularPesosCircuitosTodos(Caminhos, CaminhosPesos),
										identificarPorPeso(CaminhosPesos, Peso, Circuitos).

calcularPesosCircuitosTodos([], []).
calcularPesosCircuitosTodos([Circuito1|T], [CircuitoPeso|OutrosCircuitosPeso]) :- calculaPesoCircuito(Circuito1, CircuitoPeso),
																				calcularPesosCircuitosTodos(T, OutrosCircuitosPeso).

% retira a greenDistribution das contas
calculaPesoCircuito([H|T], [H|T]/PesoTotal) :- calculaPesoCircuitoAuxiliar(T, PesoTotal).

%calcula os pesos associados às freguesias do circuito
calculaPesoCircuitoAuxiliar([], 0).
calculaPesoCircuitoAuxiliar([Freguesia|T], PesoTotal) :- findall(Peso, entrega(_, _, Peso, _, _, Freguesia, _, _, _), ListaPesos),
														somatorio(ListaPesos, Peso1),
														calculaPesoCircuitoAuxiliar(T, Peso2),
														PesoTotal is Peso1 + Peso2.

%identifica os circuitos que tem pelo menos Peso
identificarPorPeso([], Peso, []).
identificarPorPeso([[H|T]/Peso1|Outros], Peso, [[H|T]/Peso1|Circuitos]) :- Peso1 >= Peso, !,
																		identificarPorPeso(Outros, Peso, Circuitos).

identificarPorPeso([[H|T]/Peso1|Outros], Peso, Circuitos) :- Peso1 < Peso, !,
																		identificarPorPeso(Outros, Peso, Circuitos).



%--------------------------------------------------------------------------------------------------------------------------------
% Comparar circuitos de entrega tendo em conta os indicadores de produtividade;







%--------------------------------------------------------------------------------------------------------------------------------
% Escolher o circuito mais rápido (usando o critério da distância);






faster_circuit_depth(Circuitos):- findall(
(Estafeta,Veiculo,Distancia,Tempo,Caminho),
(entrega(Estafeta, _ , Peso, Prazo, _ , NodoObjetivo, _ , _ , _ ),
melhorProfundidade(NodoObjetivo,Caminho,Distancia),
DistanciaIda is Distancia / 2,
escolheVeiculo(Peso,DistanciaIda,Veiculo,Prazo,Tempo)),Circuitos).

faster_circuit_breadth(Circuitos):- findall(
(Estafeta,Veiculo,Distancia,Tempo,Caminho),
(entrega(Estafeta, _ , Peso, Prazo, _ , NodoObjetivo, _ , _ , _ ),
melhorLargura(NodoObjetivo,Caminho,Distancia),
DistanciaIda is Distancia / 2,
escolheVeiculo(Peso,DistanciaIda,Veiculo,Prazo,Tempo)),Circuitos).


faster_circuit_limitDepth(Circuitos):- findall(
(Estafeta,Veiculo,Distancia,Tempo,Caminho),
(entrega(Estafeta, _ , Peso, Prazo, _ , NodoObjetivo, _ , _ , _ ),
melhorProfundidadeLimite(NodoObjetivo,3,Caminho,Distancia),
DistanciaIda is Distancia / 2,
escolheVeiculo(Peso,DistanciaIda,Veiculo,Prazo,Tempo)),Circuitos).




circuitoMaisRapidoDistancia(NodoObjetivo, Cam, Cus) :-  resolve_aestrela(NodoObjetivo, Cam, Cus).




%--------------------------------------------------------------------------------------------------------------------------------
% Escolher o circuito mais ecológico (usando um critério de tempo);

most_ecologic_circuit(Circuitos):- findall(
(Estafeta,Veiculo,Distancia,Tempo,Caminho),
(entrega(Estafeta,_, Peso, Prazo, _ , NodoObjetivo, _ , _ , _ ),
geraCircuitosComCustos(NodoObjetivo,Lista),
geraVeiculos(Lista,Peso,Prazo,[],Veiculos),
getMostEco(Veiculos,(Tempo,Distancia,Caminho,Veiculo))),Circuitos).


%--------------------------------------------------------------------------------------------------------------------------------
% Calculo de tempo de execução;

measure_time() :- statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
most_ecologic_circuit(X), % função a ser testada
statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
write('Execution took '), write(ExecutionTime), write(' ms.'), nl.


%--------------------------------------------------------------------------------------------------------------------------------
% Calculo da memória de execução;
measure_memory() :- statistics(global_stack,[M1,L1]),
most_ecologic_circuit(X), % função a ser testada
statistics(global_stack,[M2,L1]),
write('Used memory '), write(Memory), write(' Kb.'), nl, Memory is M2-M1.

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

minimo([(P,X)],(P,X)).
minimo([(Px,X)|L],(Py,Y)):- minimo(L,(Py,Y)), X>Y.
minimo([(Px,X)|L],(Px,X)):- minimo(L,(Py,Y)), X=<Y.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado somatorio: Lista,Somatorio -> {V,F}
% Calcula o somatório de uma lista
somatorio([], 0).
somatorio([H|T], R) :-
        somatorio(T, R2),
	R is H + R2.
