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
% 1) Identificar o estafeta que utilizou mais vezes um meio de transporte mais ecológico
% Extensão do predicado estafetaMaisVezesTransp: Veiculo, Estafeta -> {V,F}
% Predicado para procurar todas as entregas feitas por determinado veiculo e guardar o nome do estafeta numa lista
estafetaMaisVezesTransp(V, R) :-
		findall(E, entregaValida(E, V, KM, T, P, Pr, Vel, C, R, Cla, Denc, Dent), [H|T]),
		maior(H, 1, [H|T], R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 2) Identificar que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente
% Extensão do predicado estafetasEntregasCliente: Cliente, Lista encomendas, Lista de estafetas -> {V,F}
todasEntregas(C, T, R) :-
		todasEntregasDup(C, T, D),
		retiraDup(D, [], R), !.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 3) Identificar os clientes servidos por um determinado estafeta
% Extensão do predicado clientesServidosEstafeta: Estafeta, Lista Clientes -> {V,F}
clientesServidosEstafeta(E, Clientes) :-
		findall(C, entregaValida(E, V, KM, T, P, Pr, Vel, C, R, Cla, Denc, Dent), L),
		retiraDup(L, [], Clientes), !.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 4) calcular o valor faturado pela Green Distribution num determinado dia
% Extensão do predicado valorFaturado: data, resultado -> {V,F}
valorFaturado(D/M/A/_, R) :-
		findall((V, P, KM), encontraValores(D/M/A/_, (V, P, KM)), L),
 		calculaValor(L, R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 5) Identificar quais as zonas (e.g., rua ou freguesia) com maior volume de entregas por parte da Green Distribution
% Extensão do predicado volumeZona: resultado -> {V,F}
volumeZona(R) :-
		findall(Zone,entregaValida(_, _, _, _, _, _, _, _, Zone, _, _, _), Zones), % lista com todas as zonas
		group(Zones,Aux), % agrupa o número de zonas iguais indicando a zona e o número de repetições
		sort(2,@>=,Aux, Aux1), % ordenar a lista em função do número de encomendas 
		nth0(0,Aux1,R,Tail). % selecionar a cabeça da lista


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 6) Calcular a classificação média de satisfação de cliente para um determinado estafeta
% Extensão do predicado classificacaoMedia: estafeta, resultado -> {V,F}
classificacaoMedia(E, R) :-
		findall(Class, entregaValida(E, V, KM, T, P, Pr, Vel, C, Zone, Class, Denc, Dent), L),
		calculaMedia(L, R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 7) Identificar o número total de entregas pelos diferentes meios de transporte, num determinado intervalo de tempo
% Extensão do predicado totalVeiculoEntrega: data1, data2, resultado -> {V,F}
totalVeiculoEntrega(Data1,Data2,Nentregas):-
		data(Data1),
		data(Data2),
		checkPeriodo(Data1,Data2),
		veiculos(Veiculos),
		calculaVDifEntrega(Data1,Data2,[],Veiculos, Nentregas), !.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 8) Identificar o número total de entregas pelos estafetas, num determinado intervalo de tempo
% Extensão do predicado totalEntregasEstafetas: data1, data2, resultado -> {V,F}
totalEntregasEstafetas(Data1,Data2,Nentregas):-
		data(Data1),
		data(Data2), 
		checkPeriodo(Data1,Data2),
		estafetas(Estafetas), 
		calculaEstafEntregas(Data1,Data2,[],Estafetas, Nentregas),
		!.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 9) Calcular o número de encomendas entregues e não entregues pela Green Distribution, num determinado período de tempo
% Extensão do predicado numEncomendas: data1, data2, encomendasEntregues, encomendasNentregues  -> {V,F}
numEncomendas(D1, D2, Ent, NEnt) :-
		findall((Tent, TNent),(verificaPeriodo(D1, D2, (P, DEnc, DEnt)),periodoEmHoras((DEnc,DEnt),P2) ,foiEntregue((P,P2),Tent,TNent)), L),
		contaEncomendas(L,0,0,Ent, NEnt).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 10) Calcular o peso total transportado por estafeta num determinado dia
% Extensão do predicado pesoTransEstafetaDia: data, resultado -> {V,F}
pesoTransEstafetaDia(D/M/A/_, R) :- 
		findall((E, P), pesoTransEstafeta(D/M/A/_, (E, P)), L),
		agrupa(L, [], R).