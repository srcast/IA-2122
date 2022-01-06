%-------------------------------------------------------------------------
%---------------	     Predicados auxiliares     	------------------
%-------------------------------------------------------------------------

:- set_prolog_flag( single_var_warnings,off ).
:- style_check(-singleton).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado adjacente: Nodo1, Nodo2, Custo -> {V,F}
% Permite navegar bidirecionalmente no grafo
adjacente(Nodo, ProxNodo, C) :- move(Nodo, ProxNodo, C).
adjacente(Nodo, ProxNodo, C) :- move(ProxNodo, Nodo, C).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado geraVeiculosDisponiveis: Distancia, Peso, Prazo, ListaAux, VStats, Indice -> {V,F}
% Gera veículos, e tempo associado, possíveis se cumprir com os prazos estabelecidos e as restrições de peso
geraVeiculosDisponiveis(Distancia, Peso, Prazo,ListaAux,VStats,Indice) :-
    Indice == 1, Peso =< 5,
    calcula_tempo(bicicleta, Distancia, Peso, Tempo), Tempo =< Prazo, NewInd is Indice + 1,
    geraVeiculosDisponiveis(Distancia, Peso, Prazo,[(bicicleta,Tempo)|ListaAux],VStats, NewInd),
    !.
geraVeiculosDisponiveis(Distancia, Peso, Prazo,ListaAux,VStats,Indice) :-
    Indice == 1, NewInd is Indice + 1, 
    geraVeiculosDisponiveis(Distancia, Peso, Prazo,ListaAux,VStats, NewInd),
    !.
geraVeiculosDisponiveis(Distancia, Peso, Prazo,ListaAux,VStats,Indice) :-
    Indice == 2,Peso =< 20,
    calcula_tempo(mota, Distancia, Peso, Tempo),
    Tempo =< Prazo,
    NewInd is Indice + 1,
    geraVeiculosDisponiveis(Distancia, Peso, Prazo,[(mota,Tempo)|ListaAux],VStats, NewInd).
geraVeiculosDisponiveis(Distancia, Peso, Prazo,ListaAux,VStats,Indice) :-
    Indice == 2, NewInd is Indice + 1,
    geraVeiculosDisponiveis(Distancia, Peso, Prazo,ListaAux,VStats, NewInd),
    !.
geraVeiculosDisponiveis(Distancia, Peso, Prazo,ListaAux,VStats,Indice) :-
    Indice == 3, Peso =< 100,
    calcula_tempo(carro, Distancia, Peso, Tempo),
    Tempo =< Prazo, NewInd is Indice + 1,
    geraVeiculosDisponiveis(Distancia, Peso, Prazo,[(carro,Tempo)|ListaAux],VStats, NewInd),
    !.
geraVeiculosDisponiveis(Distancia, Peso, Prazo,ListaAux,VStats,Indice) :-
    Indice == 3, NewInd is Indice + 1,
    geraVeiculosDisponiveis(Distancia, Peso, Prazo,ListaAux,VStats, NewInd),
    !.
geraVeiculosDisponiveis(_,_,_,VStats,VStats,_) :- !.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado geraVeiculos: Lista1, Peso, Prazo, Lista2, Veiculos -> {V,F}
% Gerar uma lista de veículos mais ecológicos para cada circuito 
geraVeiculos([],_,_,L,L) :- !.
geraVeiculos([(Caminho,Distancia)|Caminhos], Peso, Prazo, Lista, Veiculos) :-
    DistanciaIda is Distancia / 2,
    escolheVeiculo(Peso,DistanciaIda,Veiculo,Prazo,Tempo),
    geraVeiculos(Caminhos,Peso,Prazo,[(Tempo,Distancia,Caminho,Veiculo)|Lista], Veiculos).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado getV: Veículo, Lista, Informação -> {V,F}
% Seleciona da lista o tuplo com a informação associada ao veículo indicado
getV(V, [(A1,A2,A3,V)| _] , (A1,A2,A3,V)):- !.
getV( V , [_|CS] , X ) :- getV(V,CS,X).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado getMostEco: Lista, Veículo -> {V,F}
% Obtém o veículo mais ecológico da lista
getMostEco(Lista,X):- member((_,_,_,bicicleta),Lista), getV(bicicleta,Lista,X),!.
getMostEco(Lista,X):- member((_,_,_,mota),Lista), getV(mota,Lista,X),!.
getMostEco(Lista,X):- member((_,_,_,carro),Lista), getV(carro,Lista,X).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado escolheVeiculo: Peso, Distância, Veículo, Prazo, Tempo -> {V,F}
% Escolhe veículo mais ecológico repeitando as restrições e prazo de tempo
escolheVeiculo(Peso,Distancia,Veiculo,Prazo,Tempo) :-
    Peso =< 5,
    calcula_tempo(bicicleta, Distancia, Peso, Tempo),
    Tempo =< Prazo ,
    Veiculo = bicicleta, !.
escolheVeiculo(Peso,Distancia,Veiculo,Prazo,Tempo) :-
    Peso =< 20,
    calcula_tempo(mota, Distancia, Peso, Tempo),
    Tempo =< Prazo,
    Veiculo = mota, !.
escolheVeiculo(Peso,Distancia,Veiculo,_,Tempo) :-
    Peso =< 100,
    calcula_tempo(carro, Distancia, Peso, Tempo),
    Veiculo = carro.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado calcula_tempo: Veículo, Distância, Peso, Tempo -> {V,F}
% Calcula o tempo de acordo com o veículo e suas restrições de velocidade
calcula_tempo(Veiculo,Distancia,PesoEnc, Tempo) :- 
    velMed(Veiculo,Vel),
    desconto_velocidade(Veiculo,Vel,PesoEnc, VelDesconto),
    Tempo is (Distancia / VelDesconto) + (Distancia / Vel).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado velMed: Veículo,Velocidade -> {V,F}
% Velocidade media de cada veículo
velMed(bicicleta, V) :- V is 10.
velMed(mota, V) :- V is 35.
velMed(carro, V) :- V is 25.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado desconto_velocidade: Veículo, Velocidade, Peso, NovaVelocidade -> {V,F}
% Calcula a velocidade contabilizando a pensalização associada ao peso
desconto_velocidade(bicicleta, Vel, Peso, NewVel) :- NewVel is Vel - (0.7*Peso).
desconto_velocidade(mota, Vel, Peso, NewVel) :- NewVel is Vel - (0.5*Peso).
desconto_velocidade(carro, Vel, Peso, NewVel) :- NewVel is Vel - (0.1*Peso).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado geraCircuito: Nodo, Caminho, Custo -> {V,F}
% Gera um circuito para um determinado local
geraCircuito(NodoObjetivo, Caminho, C) :-
    inicial(Inicio),
    profundidadeprimeiroInicial(NodoObjetivo, Inicio, [Inicio], [H|T], C2),
    inverso([H|T], [NodoRetirar|Resto]),
    append([H|T], Resto, Caminho),
    C is C2 * 2.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado geraCircuitosComCustos: Nodo, Circuito -> {V,F}
% Gera todos os circuito para um determinado local, com o respetivo custo
geraCircuitosComCustos(NodoObjetivo, Caminhos) :-
    findall((Caminho,C),geraCircuito(NodoObjetivo, Caminho, C), Caminhos).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado calcularPesosCircuitosTodos: Lista1, Lista2 -> {V,F}
calcularPesosCircuitosTodos([], []).
calcularPesosCircuitosTodos([Circuito1|T], [CircuitoPeso|OutrosCircuitosPeso]) :-
    calculaPesoCircuito(Circuito1, CircuitoPeso),
    calcularPesosCircuitosTodos(T, OutrosCircuitosPeso).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado calculaPesoCircuito: Lista1, Lista2 -> {V,F}
% Retira a greenDistribution das contas
calculaPesoCircuito([H|T], [H|T]/PesoTotal) :-
    calculaPesoCircuitoAuxiliar(T, PesoTotal).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado geraCircuitosComCustos: Caminho, Peso -> {V,F}
% Calcula os pesos associados às freguesias do circuito
calculaPesoCircuitoAuxiliar([], 0).
calculaPesoCircuitoAuxiliar([Freguesia|T], PesoTotal) :-
    findall(Peso, entrega(_, _, Peso, _, _, Freguesia, _, _, _), ListaPesos),
    somatorio(ListaPesos, Peso1),
    calculaPesoCircuitoAuxiliar(T, Peso2),
    PesoTotal is Peso1 + Peso2.



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado calcularNEntregasCircuitosTodos: Lista2, Lista2 -> {V,F}
calcularNEntregasCircuitosTodos([], []).
calcularNEntregasCircuitosTodos([Circuito1|T], [CircuitoPeso|OutrosCircuitosPeso]) :-
    calculaNEntregasCircuito(Circuito1, CircuitoPeso),
    calcularNEntregasCircuitosTodos(T, OutrosCircuitosPeso).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado calculaNEntregasCircuito: Lista1, Lista2 -> {V,F}
% Retira a greenDistribution das contas
calculaNEntregasCircuito([H|T], [H|T]/NEntregasTotal) :-
    calculaNEntregasCircuitoAuxiliar(T, NEntregasTotal).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado calculaNEntregasCircuitoAuxiliar: Caminho, Peso -> {V,F}
% Calcula os pesos associados às freguesias do circuito
calculaNEntregasCircuitoAuxiliar([], 0).
calculaNEntregasCircuitoAuxiliar([Freguesia|T], Nentregas) :-
    findall(Freguesia, entrega(_, _, _, _, _, Freguesia, _, _, _), Lista),
    comprimento(Lista, Nentregas1),
    calculaNEntregasCircuitoAuxiliar(T, Nentregas2),
    Nentregas is Nentregas1 + Nentregas2.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado escolheN: Lista1, Número, Lista2, Lista3 -> {V,F}
escolheN(Lista, 0, NCircuitos, Circuitos) :- reverse(NCircuitos, Circuitos), !.
escolheN([], N, NCircuitos, Circuitos) :- reverse(NCircuitos, Circuitos), !.
escolheN([H|Outros], N, NCircuitos, NCircuitosAtual) :-
    N2 is N - 1,
    adicionar(H, NCircuitos, NCircuitos2),
    escolheN(Outros, N2, NCircuitos2, NCircuitosAtual).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado ordenarCaminhos: Lista1, Lista2, Lista3 -> {V,F}
% Ordena do maior para o mais pequeno
ordenarCaminhos([],CaminhoOrdenado, CaminhoOrdenado).
ordenarCaminhos([[H|T]/Custo|Outros], CaminhoOrdenado, CaminhoFinal) :-
    ordenarCaminhosAuxiliar([H|T]/Custo, CaminhoOrdenado, CaminhoOrdenadoAtualizado),
    ordenarCaminhos(Outros, CaminhoOrdenadoAtualizado, CaminhoFinal).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado ordenarCaminhosAuxiliar: Lista1, Lista2, Lista3 -> {V,F}
% Coloca o custo no sitio correto
ordenarCaminhosAuxiliar([H|T]/Custo, [], [[H|T]/Custo]) :- !.
ordenarCaminhosAuxiliar([H|T]/Custo, [[H2|T2]/Custo2|OutrosOrdenados], [[H|T]/Custo,[H2|T2]/Custo2|OutrosOrdenados]) :-
    Custo >= Custo2, !.
ordenarCaminhosAuxiliar([H|T]/Custo, [[H2|T2]/Custo2|OutrosOrdenados2], [[H2|T2]/Custo2|OutrosOrdenados]) :-
    Custo < Custo2, !,
    ordenarCaminhosAuxiliar([H|T]/Custo, OutrosOrdenados2 ,OutrosOrdenados).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado compara_circuitos_aux2: Nodo, Estafeta, Lista, Custo1, Custo2 -> {V,F}
% Gera uma lista de todas as entregas de todos circuitos para um dado nodo
compara_circuitos_aux2(_, [], C, C) :- !.
compara_circuitos_aux2(NodoObjetivo,[(Caminho,Distancia)|R], Caux , C) :-
    findall(
        (Estafeta,NodoObjetivo,Distancia,Peso,VStats,Caminho),
        (entrega(Estafeta,_, Peso, Prazo, _ , NodoObjetivo, _ , _ , _ ),
        DistanciaIda is Distancia / 2,
        geraVeiculosDisponiveis(DistanciaIda, Peso, Prazo,[],VStats,1)), CircuitoStats),
    compara_circuitos_aux2(NodoObjetivo,R,[CircuitoStats|Caux],C).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado compara_circuitos_aux1(: Nodo, Estafeta, Lista, Custo1, Custo2 -> {V,F}
% Gera uma lista de todas as entregas de todos circuitos para um dado nodo
compara_circuitos_aux1(_,_, [], C, C) :- !.
compara_circuitos_aux1(NodoObjetivo,Estafeta,[(_,Distancia)|R], Caux , C) :-
    findall(
        (Estafeta,NodoObjetivo,Distancia,Peso,VStats),
        (entrega(Estafeta,_, Peso, Prazo, _ , NodoObjetivo, _ , _ , _ ),
        DistanciaIda is Distancia / 2,
        geraVeiculosDisponiveis(DistanciaIda, Peso, Prazo,[],VStats,1)), CircuitoStats),
    compara_circuitos_aux1(NodoObjetivo,Estafeta,R,[CircuitoStats|Caux],C).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado faster_circuits: Circuitos -> {V,F}
% Calcula o circuito mais rápido de cada entrega 
faster_circuits(Circuitos) :-
    findall(
        (Estafeta,NodoObjetivo,Distancia,Caminho),
        (entrega(Estafeta, _ ,_, _, _ , NodoObjetivo, _ , _ , _ ),
        resolve_aestrela(NodoObjetivo,Caminho/Distancia)),Circuitos).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado sortDistances: Lista1, Lista2 -> {V,F}
sortDistances([],[]).
sortDistances([A],[A]).
sortDistances([A,B|R],S) :-
	split([A,B|R],L1,L2),
	sortDistances(L1,S1),
	sortDistances(L2,S2),
	mergeDistance(S1,S2,S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado split: Lista1, Lista2, Lista3 -> {V,F}
split([],[],[]).
split([A],[A],[]).
split([A,B|Resto],[A|L],[B|R]):- split(Resto,L,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado mergeDistance: Lista1, Lista2, Lista3 -> {V,F}
mergeDistance(A,[],A).
mergeDistance([],B,B).
mergeDistance([(E,N,D,C)|R], [(E1,N1,D1,C1)|R1],[(E,N,D,C)|M]) :-
	D =< D1, mergeDistance(R,[(E1,N1,D1,C1)|R1],M).
mergeDistance([(E,N,D,C)|R], [(E1,N1,D1,C1)|R1],[(E1,N1,D1,C1)|M]) :-
	D > D1, mergeDistance([(E,N,D,C)|R],R1,M).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado retiraDestinosRepetidos: Lista1, Lista2, Lista3 -> {V,F}
retiraDestinosRepetidos([],C,C) :- !.
retiraDestinosRepetidos([(E,N,D,C)|R], [], Novo) :-
    retiraDestinosRepetidos(R, [(E,N,D,C)], Novo) ,!.
retiraDestinosRepetidos([(E,N,D,C)|R], X, Novo) :-
    \+ member((_,N,_,_),X),
    retiraDestinosRepetidos(R, [(E,N,D,C)|X], Novo) ,!.
retiraDestinosRepetidos([_|R], X, Novo) :-
    retiraDestinosRepetidos(R, X, Novo).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado most_ecologic_circuit: Circuitos -> {V,F}
% Determina o circuito mais ecológico de cada entrega 
most_ecologic_circuit(Circuitos) :-
    findall(
        (Estafeta,NodoObjetivo,Veiculo,Distancia,Tempo,Caminho),
        (entrega(Estafeta,_, Peso, Prazo, _ , NodoObjetivo, _ , _ , _ ),
        geraCircuitosComCustos(NodoObjetivo,Lista),
        geraVeiculos(Lista,Peso,Prazo,[],Veiculos),
        getMostEco(Veiculos,(Tempo,Distancia,Caminho,Veiculo))),
        Circuitos).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado get5eco: Lista1, Custo1, Lista2, Numero, Index -> {V,F}
get5eco(_,C,C,N,_) :-
    N =< 0,!.
get5eco(Lista,CAux,Circuits,Nmax,I) :-
    Nmax > 0,
    veiculoIndice(V,I),
    member((_,_,V,_,_,_),Lista),
    getListaVeiculo(V,Lista,[],ListaV),
    expandList(CAux,Nmax,ListaV, NewL,NewN),
    NewI is I + 1,
    get5eco(Lista,NewL,Circuits,NewN,NewI),
    !.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado getListaVeiculo: Veículo, Lista1, Lista2, Lista3 -> {V,F}
getListaVeiculo(_,[],L,L) :- !.
getListaVeiculo(V,[(Estafeta,NodoObjetivo,Veiculo,Distancia,Tempo,Caminho)|R],L,L1) :-
    V == Veiculo,
    getListaVeiculo(V,R,[(Estafeta,NodoObjetivo,Veiculo,Distancia,Tempo,Caminho)|L],L1),
    !.
getListaVeiculo(V,[(Estafeta,NodoObjetivo,Veiculo,Distancia,Tempo,Caminho)|R],L,L1) :-
    getListaVeiculo(V,R,L,L1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado inverso: Lista1, Lista2 -> {V,F}
inverso(Xs, Ys) :-
	inverso(Xs, [], Ys).
inverso([], Xs, Xs).
inverso([X|Xs],Ys, Zs) :-
	inverso(Xs, [X|Ys], Zs).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado seleciona: Elemento, Lista1, Lista2 -> {V,F}
seleciona(E, [E|Xs], Xs).
seleciona(E, [X|Xs], [X|Ys]) :-
	seleciona(E, Xs, Ys).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado nao: Questão -> {V,F}
nao(Questao) :-
    Questao, !, fail.
nao(Questao).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado membro: Elemento, Lista -> {V,F}
membro(X, [X|_]).
membro(X, [_|Xs]) :-
	membro(X, Xs).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado escrever: Lista -> {V,F}
escrever([]).
escrever([X|L]):- write(X), nl, escrever(L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado minimo: Lista1, Lista2, Mínimo -> {V,F}
minimo([],[X],X) :- !.
minimo([(Cam,Custo)|R],[],X) :-
minimo(R,[(Cam,Custo)],X),!.
minimo([(Cam,Custo)|R],[(Cam1,Cus1)],X) :-
	Custo < Cus1,
	minimo(R,[(Cam,Custo)],X),!.
minimo([(Cam,Custo)|R],[(Cam1,Cus1)],X) :-
	minimo(R,[(Cam1,Cus1)],X).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado expandList: CAux, Nmax, ListaV, NewL, NewN -> {V,F}
expandList(CAux, Nmax,ListaV, NewL, NewN) :-
	length(ListaV,Nacrescenta),
	(Nmax - Nacrescenta) =< 0,
	takeNList(Nmax, ListaV, CAux,NewL), NewN is 0,!.
expandList(CAux, Nmax,ListaV, NewL, NewN) :-
	length(ListaV,Nacrescenta),
	NewN is (Nmax - Nacrescenta),
	takeNList(Nacrescenta, ListaV, CAux,NewL).

% Extensão do predicado takeNList: Número, Lista1, Lista2, Lista3 -> {V,F}
takeNList(0,_,L,L) :- !.   
takeNList(N,[X|Xs],L,NewL) :-
	N > 0, N1 is N - 1,
	takeNList(N1,Xs,[X|L],NewL), !.   

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado somatorio: Lista,Somatorio -> {V,F}
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


%-------------------------------------------------------------------------
%---------------	          Profundidade             	------------------
%-------------------------------------------------------------------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado profundidade: Nodo, Caminho, Custo -> {V,F}
% Determina os caminhos disponíveis com os respetivos custos através de pesquisa em profundidade
profundidade(NodoObjetivo, CaminhoTodo, C) :-
    inicial(Inicio),
    profundidadeprimeiroInicial(NodoObjetivo, Inicio, [Inicio], Caminho, C2), !,
    duplicaCaminho(Caminho, CaminhoTodo),
	C is C2 * 2.

profundidadeprimeiroInicial(NodoObjetivo, Inicio,_, [Inicio, NodoObjetivo], C) :-
    adjacente(Inicio, NodoObjetivo, C).
profundidadeprimeiroInicial(NodoObjetivo, Inicio, Historico, [Inicio, ProxNodo|Caminho], C) :-
    adjacente(Inicio, ProxNodo, C1),
    nao(membro(ProxNodo, Historico)),
    profundidadeprimeiro(NodoObjetivo, ProxNodo, [ProxNodo|Historico], Caminho, C2),
	C is C1 + C2.

profundidadeprimeiro(NodoObjetivo, NodoAtual, _ , [NodoObjetivo], C1) :-
    adjacente(NodoAtual, NodoObjetivo, C1), !.
profundidadeprimeiro(NodoObjetivo, NodoAtual, Historico, [ProxNodo|Caminho], C) :-
    adjacente(NodoAtual, ProxNodo, C1),
    nao(membro(ProxNodo, Historico)),
    profundidadeprimeiro(NodoObjetivo, ProxNodo, [ProxNodo|Historico], Caminho, C2),
    C is C1 + C2.

% Extensão do predicado melhorProfundidade: Nodo, Caminho, Custo -> {V,F}
% Determina o melhor caminho através de pesquisa em profundidade
melhorProfundidade(NodoObjetivo, Caminho, Custo) :-
    findall(
        (Caminhos, Custos),
        profundidade(NodoObjetivo, Caminhos, Custos), L),
    minimo(L, [],(Caminho, Custo)), !.

% Extensão do predicado duplicaCaminho: Caminho1, Caminho2 -> {V,F}
% Duplica os nodos do caminho
duplicaCaminho(Caminho, CaminhoTodo) :-
    duplicaCaminhoAuxiliar(Caminho, [], CaminhoInverso),
    append(Caminho, CaminhoInverso, CaminhoTodo).

% Extensão do predicado duplicaCaminhoAuxiliar: Lista1, Lista2, Caminho -> {V,F}
% Calcula o caminho inverso, sem o nodo objetivo
duplicaCaminhoAuxiliar([], [_|T], T).
duplicaCaminhoAuxiliar([H|T], Aux, CaminhoInverso) :-
    duplicaCaminhoAuxiliar(T, [H|Aux], CaminhoInverso).

%-------------------------------------------------------------------------
%---------------	    Profundidade com limite        	------------------
%-------------------------------------------------------------------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado profundidadeLimite: Nodo, Limite, Caminho, Custo -> {V,F}
% Determina os caminhos disponíveis com os respetivos custos através de pesquisa em profundidade com limite
% profundidadeLimite(crespos,3,Caminho,Custo).
profundidadeLimite(NodoObjetivo, Limite, CaminhoTodo, C) :-
    inicial(Inicio),
    profundidadeprimeiroLimiteInicial(NodoObjetivo, Limite, Inicio, [Inicio], Caminho, C2), !,
    duplicaCaminho(Caminho, CaminhoTodo),
    C is C2 * 2.

profundidadeprimeiroLimiteInicial(NodoObjetivo,_, Inicio,_, [Inicio, NodoObjetivo], C) :-
    adjacente(Inicio, NodoObjetivo, C).
profundidadeprimeiroLimiteInicial(NodoObjetivo, Limite, Inicio, Historico, [Inicio, ProxNodo|Caminho], C) :-
    adjacente(Inicio, ProxNodo, C1),
    nao(membro(ProxNodo, Historico)),
    length([ProxNodo|Historico], Tam),
    Tam - 1 < Limite,
    profundidadeprimeiroLimite(NodoObjetivo, Limite, ProxNodo, [ProxNodo|Historico], Caminho, C2),
    C is C1 + C2.

profundidadeprimeiroLimite(NodoObjetivo, _, NodoAtual, _, [NodoObjetivo], C1) :-
    adjacente(NodoAtual, NodoObjetivo, C1), !.
profundidadeprimeiroLimite(NodoObjetivo, Limite, NodoAtual, Historico, [ProxNodo|Caminho], C) :-
    adjacente(NodoAtual, ProxNodo, C1),
    nao(membro(ProxNodo, Historico)),
    length([ProxNodo|Historico], Tam),
    Tam - 1 < Limite,
    profundidadeprimeiroLimite(NodoObjetivo, Limite, ProxNodo, [ProxNodo|Historico], Caminho, C2),
    C is C1 + C2.

% Extensão do predicado melhorProfundidadeLimite: Nodo, Limite, Caminho, Custo -> {V,F}
% Determina o melhor caminho através de pesquisa em profundidade com limite  
melhorProfundidadeLimite(NodoObjetivo, Limite, Caminho, Custo) :-
    findall((SS, CC), profundidadeLimite(NodoObjetivo, Limite, SS, CC), L),
    minimo(L, (Caminho, [],Custo)), !.

%-------------------------------------------------------------------------
%---------------	          Largura               	------------------
%-------------------------------------------------------------------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado largura: Nodo1, Nodo2, Custo -> {V,F}
% Determina os caminhos disponíveis com os respetivos custos através de pesquisa em largura
% largura(tadim,Caminho,Custo).
largura(Dest, Caminho, Custos) :-
    inicial(Orig),
    largura2(Dest,[[Orig/0]],Cam),
    !,
    somaCustos(Cam, Custos2),
    retiraCustos(Cam, [NodoRetirar|Resto]),
    inverso([NodoRetirar|Resto], CaminhoIncompleto),
    append(CaminhoIncompleto, Resto, Caminho),
    Custos is Custos2 * 2.

% Extensão do predicado largura2: Nodo, Lista1, Lista2 -> {V,F}
largura2(Dest,[[Dest/Cus|T]|_],[Dest/Cus|T]).
largura2(Dest,[LA|Outros],Cam) :-
    LA=[Act|_],
    Act = Atual/Cus,
    findall(
        [X/C|LA],
        (Dest\==Atual,adjacente(Atual,X, C),
        \+membroCustos(X,LA)),Novos),
    append(Outros,Novos,Todos),
    largura2(Dest,Todos,Cam).

% Extensão do predicado somaCustos: Lista, Custo -> {V,F}
somaCustos([], 0).
somaCustos([_/Custo|Resto], Custos) :-
    somaCustos(Resto, Custos2),
    Custos is Custo + Custos2.

% Extensão do predicado somaCustos: Lista1, Lista2 -> {V,F}
retiraCustos([], []).
retiraCustos([Localidade/_|Resto], [Localidade|Outras]) :- retiraCustos(Resto, Outras).

% Extensão do predicado somaCustos: Lista1, Lista2 -> {V,F}
membroCustos(Local, [Local/Cus|Resto]).
membroCustos(Local, [Nodo/Cus|Resto]) :-
    membroCustos(Local, Resto).

%-------------------------------------------------------------------------
%---------------	          Gulosa                	------------------
%-------------------------------------------------------------------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado resolve_gulosa: Nodo, Lista -> {V,F}
% Determina os caminhos disponíveis com os respetivos custos através de pesquisa gulosa
% resolve_gulosa(fraiao,Caminhos).
resolve_gulosa(Nodo, Caminho/Custo) :-
	estima(Nodo, Estima),
	agulosa([[Nodo]/0/Estima], [GD|T]/Custo2/_), !,
	inverso([GD|T], [NodoRetirar|CaminhoInverso]),
	append([GD|T], CaminhoInverso, Caminho), %junta sem o NodoObjetivo duplicado
	Custo is Custo2 * 2.

% Extensão do predicado agulosa: Lista1, Lista2 -> {V,F}
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

% Extensão do predicado obtem_melhor_g: Lista1, Lista2 -> {V,F}
obtem_melhor_g([Caminho], Caminho) :- !.
obtem_melhor_g([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos], MelhorCaminho) :-
	Est1 =< Est2, !, 
	obtem_melhor_g([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho).
obtem_melhor_g([_|Caminhos], MelhorCaminho) :- 
	obtem_melhor_g(Caminhos, MelhorCaminho).

% Extensão do predicado expande_gulosa: Lista1, Lista2 -> {V,F}
expande_gulosa(Caminho, ExpCaminhos) :-
	findall(NovoCaminho, adjacente2(Caminho,NovoCaminho), ExpCaminhos).	


%-------------------------------------------------------------------------
%---------------	             A*                 	------------------
%-------------------------------------------------------------------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado resolve_aestrela: Nodo, Lista -> {V,F}
% Determina os caminhos disponíveis com os respetivos custos através de pesquisa A*
% resolve_aestrela(fraiao,Caminho).
resolve_aestrela(Nodo, Caminho/Custo) :-
	estima(Nodo, Estima),
	aestrela([[Nodo]/0/Estima], [GD|T]/Custo2/_), !,
	inverso([GD|T], [NodoRetirar|CaminhoInverso]),
	append([GD|T], CaminhoInverso, Caminho), 
	Custo is Custo2 * 2.

% Extensão do predicado aestrela: Lista1, Lista2 -> {V,F}
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

% Extensão do predicado obtem_melhor: Lista1, Lista2 -> {V,F}
obtem_melhor([Caminho], Caminho) :- !.
obtem_melhor([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos], MelhorCaminho) :-
	Custo1 + Est1 =< Custo2 + Est2, !,
	obtem_melhor([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho). 
obtem_melhor([_|Caminhos], MelhorCaminho) :- 
	obtem_melhor(Caminhos, MelhorCaminho).

% Extensão do predicado expande_aestrela: Lista1, Lista2 -> {V,F}
expande_aestrela(Caminho, ExpCaminhos) :-
	findall(NovoCaminho, adjacente2(Caminho,NovoCaminho), ExpCaminhos).

% Extensão do predicado adjacente2: Lista1, Lista2 -> {V,F}
adjacente2([Nodo|Caminho]/Custo/_, [ProxNodo,Nodo|Caminho]/NovoCusto/Est) :-
	move(Nodo, ProxNodo, PassoCusto),
	\+member(ProxNodo, Caminho),
	NovoCusto is Custo + PassoCusto,
	estima(ProxNodo, Est).