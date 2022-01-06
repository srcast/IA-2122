%COLOCAR NA MAIN DEPOIS
:- set_prolog_flag( single_var_warnings,off ).
:- style_check(-singleton).

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
%move(semelhe,trandeiras,8).
move(trandeiras,real,2).
move(real,saoVicente,5).
move(real,greenDistribution,8).
move(saoVicente,pedralva,6).
move(saoVicente,tenoes,4).
move(pedralva,priscos,7).
move(tenoes,cividade,2).
move(tenoes,priscos,1).
%move(priscos,padimDaGraca,4).
%move(cividade,crespos,9).
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



%--------------------------------- Predicados relacionados ao veículo - - - - - - - - - -  -  -  -  -   -



% Gera Veiculos e tempo associado possíveis se cumprir com o prazo estabelecido restrições de peso
geraVeiculosDisponiveis(Distancia, Peso, Prazo,ListaAux,VStats,Indice):- Indice == 1, Peso =< 5 , calcula_tempo(bicicleta, Distancia, Peso, Tempo), Tempo =< Prazo, NewInd is Indice + 1, geraVeiculosDisponiveis(Distancia, Peso, Prazo,[(bicicleta,Tempo)|ListaAux],VStats, NewInd),!.
geraVeiculosDisponiveis(Distancia, Peso, Prazo,ListaAux,VStats,Indice):- Indice == 1, NewInd is Indice + 1, geraVeiculosDisponiveis(Distancia, Peso, Prazo,ListaAux,VStats, NewInd),!.

geraVeiculosDisponiveis(Distancia, Peso, Prazo,ListaAux,VStats,Indice):- Indice == 2,Peso =< 20 , calcula_tempo(mota, Distancia, Peso, Tempo), Tempo =< Prazo, NewInd is Indice + 1, geraVeiculosDisponiveis(Distancia, Peso, Prazo,[(mota,Tempo)|ListaAux],VStats, NewInd).
geraVeiculosDisponiveis(Distancia, Peso, Prazo,ListaAux,VStats,Indice):- Indice == 2,NewInd is Indice + 1, geraVeiculosDisponiveis(Distancia, Peso, Prazo,ListaAux,VStats, NewInd),!.

geraVeiculosDisponiveis(Distancia, Peso, Prazo,ListaAux,VStats,Indice):- Indice == 3,Peso =< 100 , calcula_tempo(carro, Distancia, Peso, Tempo), Tempo =< Prazo, NewInd is Indice + 1, geraVeiculosDisponiveis(Distancia, Peso, Prazo,[(carro,Tempo)|ListaAux],VStats, NewInd),!.
geraVeiculosDisponiveis(Distancia, Peso, Prazo,ListaAux,VStats,Indice):- Indice == 3, NewInd is Indice + 1, geraVeiculosDisponiveis(Distancia, Peso, Prazo,ListaAux,VStats, NewInd),!.

geraVeiculosDisponiveis(_,_,_,VStats,VStats,_):-!.




% Predicado responsável gerar uma lista de veículos mais ecológicos para cada circuito
% return => [(Veiculo,Tempo,Distancia),...] 
geraVeiculos([],_,_,L,L):-!.
geraVeiculos([(Caminho,Distancia)|Caminhos], Peso, Prazo, Lista, Veiculos):- DistanciaIda is Distancia / 2, escolheVeiculo(Peso,DistanciaIda,Veiculo,Prazo,Tempo),
																geraVeiculos(Caminhos,Peso,Prazo,[(Tempo,Distancia,Caminho,Veiculo)|Lista], Veiculos).



% Obtém da lista o componente que possui a informação associada ao veículo indicado
getV(V, [(A1,A2,A3,V)| _] , (A1,A2,A3,V)):- !.
getV( V , [_|CS] , X ) :- getV(V,CS,X).

% Obtém da lista de veículos aquele que é mais ecológico
getMostEco(Lista,X):- member((_,_,_,bicicleta),Lista), getV(bicicleta,Lista,X),!.
getMostEco(Lista,X):- member((_,_,_,mota),Lista), getV(mota,Lista,X),!.
getMostEco(Lista,X):- member((_,_,_,carro),Lista), getV(carro,Lista,X).


% Escolhe veículo mais ecológico repeitando as restrições e prazo de tempo
escolheVeiculo(Peso,Distancia,Veiculo,Prazo,Tempo):- Peso =< 5 , calcula_tempo(bicicleta, Distancia, Peso, Tempo) , Tempo =< Prazo , Veiculo = bicicleta, !.
escolheVeiculo(Peso,Distancia,Veiculo,Prazo,Tempo):- Peso =< 20 , calcula_tempo(mota, Distancia, Peso, Tempo) , Tempo =< Prazo, Veiculo = mota, !.
escolheVeiculo(Peso,Distancia,Veiculo,_,Tempo):- Peso =< 100 , calcula_tempo(carro, Distancia, Peso, Tempo) , Veiculo = carro.

%>

% Calcula o tempo de acordo com o veículo e suas restrições de velocidade
calcula_tempo(Veiculo,Distancia,PesoEnc, Tempo) :-  velMed(Veiculo,Vel),desconto_velocidade(Veiculo,Vel,PesoEnc, VelDesconto), Tempo is (Distancia / VelDesconto) + (Distancia / Vel).

% Velocidade media de cada veículo
velMed(bicicleta, V):- V is 10.
velMed(mota, V):- V is 35.
velMed(carro, V):- V is 25.

% Calcula a Velocidade descontando o peso
desconto_velocidade(bicicleta, Vel, Peso, NewVel) :- NewVel is Vel - (0.7*Peso).
desconto_velocidade(mota, Vel, Peso, NewVel) :- NewVel is Vel - (0.5*Peso).
desconto_velocidade(carro, Vel, Peso, NewVel) :- NewVel is Vel - (0.1*Peso).




%----------------------------------------------------------------------------------------------------------------------------------------------
%----------------------------------------------------------- Distancia ------------------------------------------------------------------------
%----------------------------------------------------------------------------------------------------------------------------------------------

%----------------------- Profundidade ---------------------------------------------------------------------------------------------------------


%executar esta
profundidade(NodoObjetivo, CaminhoTodo, C) :- inicial(Inicio),
										profundidadeprimeiroInicial(NodoObjetivo, Inicio, [Inicio], Caminho, C2),!,
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
													minimo(L, [],(Caminho, Custo)), !.







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
							minimo(L, (Caminho, [],Custo)), !.


%----------------------- Largura ---------------------------------------------------------------------------------------------------------



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

resolve_gulosa(Nodo, Caminho/Custo) :-
	estima(Nodo, Estima),
	agulosa([[Nodo]/0/Estima], [GD|T]/Custo2/_), !,
	inverso([GD|T], [NodoRetirar|CaminhoInverso]),
	append([GD|T], CaminhoInverso, Caminho), %junta sem o NodoObjetivo duplicado
	Custo is Custo2 * 2.

agulosa(Caminhos, Caminho) :-
	obtem_melhor_g(Caminhos, Caminho),
	Caminho = [Nodo|_]/_/_,
	objetivo(Nodo).

agulosa(Caminhos, SolucaoCaminho) :-
	obtem_melhor_g(Caminhos, MelhorCaminho),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expande_gulosa(MelhorCaminho, ExpCaminhos),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    agulosa(NovoCaminhos, SolucaoCaminho).		

obtem_melhor_g([Caminho], Caminho) :- !.

obtem_melhor_g([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos], MelhorCaminho) :-
	Est1 =< Est2, !,      %>
	obtem_melhor_g([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho).
	
obtem_melhor_g([_|Caminhos], MelhorCaminho) :- 
	obtem_melhor_g(Caminhos, MelhorCaminho).

expande_gulosa(Caminho, ExpCaminhos) :-
	findall(NovoCaminho, adjacente2(Caminho,NovoCaminho), ExpCaminhos).	







%-----------------------  A* ---------------------------------------------------------------------------------------------------------

resolve_aestrela(Nodo, Caminho/Custo) :-
	estima(Nodo, Estima),
	aestrela([[Nodo]/0/Estima], [GD|T]/Custo2/_), !,
	inverso([GD|T], [NodoRetirar|CaminhoInverso]),
	append([GD|T], CaminhoInverso, Caminho), %junta sem o NodoObjetivo duplicado
	Custo is Custo2 * 2.

aestrela(Caminhos, Caminho) :-
	obtem_melhor(Caminhos, Caminho),
	Caminho = [Nodo|_]/_/_,
	objetivo(Nodo).

aestrela(Caminhos, SolucaoCaminho) :-
	obtem_melhor(Caminhos, MelhorCaminho),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expande_aestrela(MelhorCaminho, ExpCaminhos),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    aestrela(NovoCaminhos, SolucaoCaminho).	

obtem_melhor([Caminho], Caminho) :- !.
obtem_melhor([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos], MelhorCaminho) :-
	Custo1 + Est1 =< Custo2 + Est2, !,   %>
	obtem_melhor([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho). 
obtem_melhor([_|Caminhos], MelhorCaminho) :- 
	           obtem_melhor(Caminhos, MelhorCaminho).



expande_aestrela(Caminho, ExpCaminhos) :-
	findall(NovoCaminho, adjacente2(Caminho,NovoCaminho), ExpCaminhos).


adjacente2([Nodo|Caminho]/Custo/_, [ProxNodo,Nodo|Caminho]/NovoCusto/Est) :-
	move(Nodo, ProxNodo, PassoCusto),
	\+member(ProxNodo, Caminho),
	NovoCusto is Custo + PassoCusto,
	estima(ProxNodo, Est).











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

%executar esta
%identificar N circuitos com mais peso
identificarCircuitosPeso(N, Circuitos) :- geraCircuitosObjetivo(NodoObjetivo, Caminhos),
										calcularPesosCircuitosTodos(Caminhos, [H|T]),
										ordenarCaminhos(T, [H], CircuitosOrd),
										escolheN(CircuitosOrd, N, [], Circuitos).

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





%executar esta
%identifar os N circuitos com mais entregas
identificarCircuitosNEntregas(N, Circuitos) :- geraCircuitosObjetivo(NodoObjetivo, Caminhos),
														calcularNEntregasCircuitosTodos(Caminhos, [H|T]),
														ordenarCaminhos(T, [H], CircuitosOrd),
														escolheN(CircuitosOrd, N, [], Circuitos).

calcularNEntregasCircuitosTodos([], []).
calcularNEntregasCircuitosTodos([Circuito1|T], [CircuitoPeso|OutrosCircuitosPeso]) :- calculaNEntregasCircuito(Circuito1, CircuitoPeso),
																				calcularNEntregasCircuitosTodos(T, OutrosCircuitosPeso).

% retira a greenDistribution das contas
calculaNEntregasCircuito([H|T], [H|T]/NEntregasTotal) :- calculaNEntregasCircuitoAuxiliar(T, NEntregasTotal).

%calcula os pesos associados às freguesias do circuito
calculaNEntregasCircuitoAuxiliar([], 0).
calculaNEntregasCircuitoAuxiliar([Freguesia|T], Nentregas) :- findall(Freguesia, entrega(_, _, _, _, _, Freguesia, _, _, _), Lista),
															comprimento(Lista, Nentregas1),
															calculaNEntregasCircuitoAuxiliar(T, Nentregas2),
															Nentregas is Nentregas1 + Nentregas2.





%escolhe os N elementos da lista
escolheN(Lista, 0, NCircuitos, Circuitos) :- reverse(NCircuitos, Circuitos), !.
escolheN([], N, NCircuitos, Circuitos) :- reverse(NCircuitos, Circuitos), !.

escolheN([H|Outros], N, NCircuitos, NCircuitosAtual) :- N2 is N - 1, 
														adicionar(H, NCircuitos, NCircuitos2),
 											            escolheN(Outros, N2, NCircuitos2, NCircuitosAtual).




%ordena do maior para o mais pequeno
ordenarCaminhos([],CaminhoOrdenado, CaminhoOrdenado).
ordenarCaminhos([[H|T]/Custo|Outros], CaminhoOrdenado, CaminhoFinal) :- ordenarCaminhosAuxiliar([H|T]/Custo, CaminhoOrdenado, CaminhoOrdenadoAtualizado),
														ordenarCaminhos(Outros, CaminhoOrdenadoAtualizado, CaminhoFinal).


%coloca o custo no sitio correto
ordenarCaminhosAuxiliar([H|T]/Custo, [], [[H|T]/Custo]) :- !.

ordenarCaminhosAuxiliar([H|T]/Custo, [[H2|T2]/Custo2|OutrosOrdenados], [[H|T]/Custo,[H2|T2]/Custo2|OutrosOrdenados]) :- Custo >= Custo2, !.

ordenarCaminhosAuxiliar([H|T]/Custo, [[H2|T2]/Custo2|OutrosOrdenados2], [[H2|T2]/Custo2|OutrosOrdenados]) :- Custo < Custo2, !,
																					ordenarCaminhosAuxiliar([H|T]/Custo, OutrosOrdenados2 ,OutrosOrdenados).











%--------------------------------------------------------------------------------------------------------------------------------
% Comparar circuitos de entrega tendo em conta os indicadores de produtividade;

% Gera uma lista de todas as entregas de todos circuitos para um NodoObjetivo, sendo a lista de retorno = [(Estafeta,NodoObjetivo,DistanciaTotal,PesoDaEntrega,[lista de veiculos disponiveis em conjunto do tempo calculado],Circuito), ...]
% como correr => compara_circuitos_estafeta((cidade que há entrega),Circuitos)
% exemplo bom para correr: compara_circuitos_estafeta(crespos,C).
compara_circuitos(NodoObjetivo,Circuitos):- geraCircuitosComCustos(NodoObjetivo,Circuitos1), compara_circuitos_aux2(NodoObjetivo,Circuitos1,[],Circuitos).

compara_circuitos_aux2(_, [], C, C):- !.
compara_circuitos_aux2(NodoObjetivo,[(Caminho,Distancia)|R], Caux , C):- findall(
(Estafeta,NodoObjetivo,Distancia,Peso,VStats,Caminho),
(entrega(Estafeta,_, Peso, Prazo, _ , NodoObjetivo, _ , _ , _ ),
DistanciaIda is Distancia / 2,
geraVeiculosDisponiveis(DistanciaIda, Peso, Prazo,[],VStats,1)), CircuitoStats),
compara_circuitos_aux2(NodoObjetivo,R,[CircuitoStats|Caux],C).


% Gera uma lista de todas as entregas de todos circuitos para um NodoObjetivo que uma Estafeta específica realizou, sendo a lista de retorno = [(Estafeta,NodoObjetivo,DistanciaTotal,PesoDaEntrega,[lista de veiculos disponiveis em conjunto do tempo calculado]), ...]
% como correr => compara_circuitos_estafeta((cidade que há entrega), (estafeta que realiza entrega à cidade),Circuitos)
% exemplo bom para correr: compara_circuitos_estafeta(adaufe,marco,C).
compara_circuitos_estafeta(NodoObjetivo,Estafeta,Circuitos):- geraCircuitosComCustos(NodoObjetivo,Circuitos1), compara_circuitos_aux1(NodoObjetivo,Estafeta,Circuitos1,[],Circuitos).

compara_circuitos_aux1(_,_, [], C, C):- !.
compara_circuitos_aux1(NodoObjetivo,Estafeta,[(_,Distancia)|R], Caux , C):- findall(
(Estafeta,NodoObjetivo,Distancia,Peso,VStats),
(entrega(Estafeta,_, Peso, Prazo, _ , NodoObjetivo, _ , _ , _ ),
DistanciaIda is Distancia / 2,
geraVeiculosDisponiveis(DistanciaIda, Peso, Prazo,[],VStats,1)), CircuitoStats),
compara_circuitos_aux1(NodoObjetivo,Estafeta,R,[CircuitoStats|Caux],C).








%--------------------------------------------------------------------------------------------------------------------------------
% Escolher o circuito mais rápido (usando o critério da distância);

% <----- Predicados principais ------->

% predicado que calcula os circuitos mais rápidos ordenamente de cada destino encontrado nas entregas 
top_faster_circuits(Circuits):- faster_circuits(Cs), retiraDestinosRepetidos(Cs,[],NewCs), sortDistances(NewCs,Circuits) .	

% predicado que calcula o circuito mais rápido de cada entrega 
faster_circuits(Circuitos):- findall(
(Estafeta,NodoObjetivo,Distancia,Caminho),
(entrega(Estafeta, _ ,_, _, _ , NodoObjetivo, _ , _ , _ ),
resolve_aestrela(NodoObjetivo,Caminho/Distancia)),Circuitos).


% <---  Predicados auxiliares dos predicados principais --->

sortDistances([],[]).
sortDistances([A],[A]).
sortDistances([A,B|R],S):-
	split([A,B|R],L1,L2),
	sortDistances(L1,S1),
	sortDistances(L2,S2),
	mergeDistance(S1,S2,S).

split([],[],[]).
split([A],[A],[]).
split([A,B|Resto],[A|L],[B|R]):- split(Resto,L,R).

mergeDistance(A,[],A).
mergeDistance([],B,B).
mergeDistance([(E,N,D,C)|R], [(E1,N1,D1,C1)|R1],[(E,N,D,C)|M]):-
	D =< D1, mergeDistance(R,[(E1,N1,D1,C1)|R1],M).
mergeDistance([(E,N,D,C)|R], [(E1,N1,D1,C1)|R1],[(E1,N1,D1,C1)|M]):-
	D > D1, mergeDistance([(E,N,D,C)|R],R1,M).



retiraDestinosRepetidos([],C,C):-!.
retiraDestinosRepetidos([(E,N,D,C)|R], [], Novo):- retiraDestinosRepetidos(R, [(E,N,D,C)], Novo) ,!.
retiraDestinosRepetidos([(E,N,D,C)|R], X, Novo):- \+ member((_,N,_,_),X),retiraDestinosRepetidos(R, [(E,N,D,C)|X], Novo) ,!.
retiraDestinosRepetidos([_|R], X, Novo):- retiraDestinosRepetidos(R, X, Novo).



%--------------------------------------------------------------------------------------------------------------------------------
% Escolher o circuito mais ecológico (usando um critério de tempo);

% --> Predicados principais <---

% determina os top N circuitos mais ecológicos , retorno => [(Estafeta,cidadeObjetivo,Veiculo mais ecológico para a entrega, Distancia, Tempo feito pelo veículo,Caminho),...]
% como correr => toN_most_eco((Circuitos a ser calculado), (número de melhores circuitos))
% exemplo de como correr => toN_most_eco(Circuitos, 5).
topN_most_eco(Circuits, N):- most_ecologic_circuit(Cs), get5eco(Cs,[],Circuits,N,1).


% determina o circuito mais ecológico de cada entrega , retorno => [(Estafeta,cidadeObjetivo,Veiculo mais ecológico para a entrega, Distancia, Tempo feito pelo veículo,Caminho),...]
most_ecologic_circuit(Circuitos):- findall(
(Estafeta,NodoObjetivo,Veiculo,Distancia,Tempo,Caminho),
(entrega(Estafeta,_, Peso, Prazo, _ , NodoObjetivo, _ , _ , _ ),
geraCircuitosComCustos(NodoObjetivo,Lista),
geraVeiculos(Lista,Peso,Prazo,[],Veiculos),
getMostEco(Veiculos,(Tempo,Distancia,Caminho,Veiculo))),Circuitos).


% --> Predicados auxiliares <---

get5eco(_,C,C,N,_) :- N =< 0,!.
get5eco(Lista,CAux,Circuits,Nmax,I) :-  
Nmax > 0 , 
veiculoIndice(V,I), 
member((_,_,V,_,_,_),Lista), 
getListaVeiculo(V,Lista,[],ListaV),
expandList(CAux,Nmax,ListaV, NewL,NewN),
NewI is I + 1,
get5eco(Lista,NewL,Circuits,NewN,NewI),!.

getListaVeiculo(_,[],L,L):-!.
getListaVeiculo(V,[(Estafeta,NodoObjetivo,Veiculo,Distancia,Tempo,Caminho)|R],L,L1):- V == Veiculo, getListaVeiculo(V,R,[(Estafeta,NodoObjetivo,Veiculo,Distancia,Tempo,Caminho)|L],L1),!.
getListaVeiculo(V,[(Estafeta,NodoObjetivo,Veiculo,Distancia,Tempo,Caminho)|R],L,L1):- getListaVeiculo(V,R,L,L1).

veiculoIndice(bicicleta,1).
veiculoIndice(mota,2).
veiculoIndice(carro,3).



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
statistics(global_stack,[M2,L2]),
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



minimo([],[X],X):- !.
minimo([(Cam,Custo)|R],[],X):- minimo(R,[(Cam,Custo)],X),!.
minimo([(Cam,Custo)|R],[(Cam1,Cus1)],X):- Custo < Cus1, minimo(R,[(Cam,Custo)],X),!.
minimo([(Cam,Custo)|R],[(Cam1,Cus1)],X):- minimo(R,[(Cam1,Cus1)],X).


expandList(CAux, Nmax,ListaV, NewL, NewN):- length(ListaV,Nacrescenta), (Nmax - Nacrescenta) =< 0 , takeNList(Nmax, ListaV, CAux,NewL), NewN is 0,!.
expandList(CAux, Nmax,ListaV, NewL, NewN):- length(ListaV,Nacrescenta), NewN is (Nmax - Nacrescenta), takeNList(Nacrescenta, ListaV, CAux,NewL).

takeNList(0,_,L,L):- !.   
takeNList(N,[X|Xs],L,NewL):- N > 0, N1 is N - 1 , takeNList(N1,Xs,[X|L],NewL),!.   



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado somatorio: Lista,Somatorio -> {V,F}
% Calcula o somatório de uma lista
somatorio([], 0).
somatorio([H|T], R) :-
        somatorio(T, R2),
	R is H + R2.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado comprimento: Lista,Comprimento -> {V,F}

comprimento( [],0 ).
comprimento( [X|L],N ) :-
    comprimento( L,N1 ),
    N is N1+1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado adicionar: Elemento,Lista,Resultado -> {V,F}

adicionar(X,[],[X]) :- !.
adicionar(X,[H|T],[X,H|T]) :- !.