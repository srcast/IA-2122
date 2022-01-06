%-------------------------------------------------------------------------
%---------------	Inteligência Artificial -> Parte I	------------------
%-------------------------------------------------------------------------

%-------------------------------------------------------------------------
%-------------	Elementos do Grupo			------------------------------
%-------			Pedro Aquino Martins de Araújo, A90614			------
%-------			Ricardo Miguel Santos Gomes, A93785				------
%-------			Rui Pedro Gomes Coelho, A58898					------
%-------			Vasco Baptista Moreno, A94194					------
%-------------------------------------------------------------------------

:- set_prolog_flag( single_var_warnings,off ).
:- style_check(-singleton).


% Consulta de ficheiros
:- consult('base_conhecimento.pl').
:- consult('predicados_auxiliares.pl').

%-------------------------------------------------------------------------
%---------------	            Queries               	------------------
%-------------------------------------------------------------------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 1) Gerar os circuitos de entrega, caso existam, que cubram um determinado território
% Extensão do predicado geraCircuitosObjetivo: Nodo, Caminhos -> {V,F}
% geraCircuitosObjetivo(saoVicente, Caminhos) 
geraCircuitosObjetivo(NodoObjetivo, Caminhos) :-
	findall(Caminho,geraCircuito(NodoObjetivo, Caminho, C), Caminhos).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 2) Identificar quais os circuitos com maior número de entregas 
% Extensão do predicado identificarCircuitosPeso: TopN, Circuitos -> {V,F}
% Identifica os TopN circuitos com maior peso de entregas
% identificarCircuitosPeso(3,Circuitos).
identificarCircuitosPeso(N, Circuitos) :-
	geraCircuitosObjetivo(NodoObjetivo, Caminhos),
	calcularPesosCircuitosTodos(Caminhos, [H|T]),
	ordenarCaminhos(T, [H], CircuitosOrd),
	escolheN(CircuitosOrd, N, [], Circuitos).

% Extensão do predicado identificarCircuitosNEntregas: TopN, Circuitos -> {V,F}
% Identifica os TopN circuitos com maior número de entregas
% identificarCircuitosNEntregas(3,Circuitos).
identificarCircuitosNEntregas(N, Circuitos) :-
	geraCircuitosObjetivo(NodoObjetivo, Caminhos),
	calcularNEntregasCircuitosTodos(Caminhos, [H|T]),
	ordenarCaminhos(T, [H], CircuitosOrd),
	escolheN(CircuitosOrd, N, [], Circuitos).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 3) Comparar circuitos de entrega tendo em conta os indicadores de produtividade
% Extensão do predicado compara_circuitos: Nodo, Caminhos -> {V,F}
% Compara os circuitos em função da distância percorrida
% compara_circuitos_estafeta(crespos,Circuitos).
compara_circuitos(NodoObjetivo,Circuitos) :-
	geraCircuitosComCustos(NodoObjetivo,Circuitos1),
	compara_circuitos_aux2(NodoObjetivo,Circuitos1,[],Circuitos).


% Extensão do predicado compara_circuitos_estafeta: Nodo, Estafeta, Caminhos -> {V,F}
% Compara os circuitos em função do tempo de entrega
% compara_circuitos_estafeta(adaufe,marco,Circuitos).
compara_circuitos_estafeta(NodoObjetivo,Estafeta,Circuitos) :-
	geraCircuitosComCustos(NodoObjetivo,Circuitos1),
	compara_circuitos_aux1(NodoObjetivo,Estafeta,Circuitos1,[],Circuitos).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 4) Escolher o circuito mais rápido
% Extensão do predicado top_faster_circuits: Caminhos -> {V,F}
% Determina os circuitos de entrega mais curtos
% top_faster_circuits(Circuits)
top_faster_circuits(Circuits) :-
	faster_circuits(Cs),
	retiraDestinosRepetidos(Cs,[],NewCs),
	sortDistances(NewCs,Circuits).	

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 5) Escolher o circuito mais ecológico
% Extensão do predicado topN_most_eco: Caminhos, TopN -> {V,F}
% Determina os TopN circuitos de entrega mais ecológicos
% topN_most_eco(Circuits, 3). 
topN_most_eco(Circuits, N) :-
	most_ecologic_circuit(Cs),
	get5eco(Cs,[],Circuits,N,1).

%-------------------------------------------------------------------------
%---------------	          Performance              	------------------
%-------------------------------------------------------------------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Calculo de tempo de execução de um predicado
measure_time() :-
	statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
	topN_most_eco(Circuits, 3), % predicado a ser testado
	statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
	write('Execution took '), write(ElapsedTime), write(' ms.'), nl,
	ElapsedTime is NewTimeSinceStart-TimeSinceStart.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Calculo da memória de execução de um predicado
measure_memory() :-
	statistics(global_stack,[M1,L1]),
	topN_most_eco(Circuits, 3), % predicado a ser testado
	statistics(global_stack,[M2,L2]),
	write('Used memory '), write(Memory), write(' Kb.'), nl,
	Memory is M2-M1.