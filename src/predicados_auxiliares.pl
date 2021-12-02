%-------------------------------------------------------------------------
%---------------	     Predicados auxiliares         	------------------
%-------------------------------------------------------------------------

:- set_prolog_flag( single_var_warnings,off ).
:- style_check(-singleton).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado entregaValida: estafeta, veiculo, km, tipoEncomenda, pesoEnc, prazo, velocidade, cliente, rua, classificacao, data encomenda, data entrega -> {V,F}
% Verifica a validade de uma entrega
entregaValida(E, V, KM, T, P, Pr, Vel, C, R, Cla, Denc, Dent) :-
        entrega(E, V, KM, T, P, Pr, Vel, C, R, Cla, Denc, Dent),
	veiculo(V, P, Vel),
        classificacao(Cla),
        morada(C, R, KM),
        data(Denc),
        data(Dent).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado veiculo: veiculo, peso, velocidade -> {V,F}
% Verifica se as características do veículo respeitam as regras
veiculo(bicicleta, P, V) :-
        integer(P) =< 5, !,
        V =:= 10, !.
veiculo(mota, P, V) :-
        integer(P) =< 20, !,
        V =:= 35, !.
veiculo(carro, P, V) :-
        integer(P) =< 100, !,
        V =:= 25, !.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado classificacao: classificacao -> {V,F}
% Valida a classificação
classificacao(C) :-
        member(C, [0,1,2,3,4,5]), !.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado data: dia, mes, ano, hora -> {V,F}
% Verifica se uma data é válida
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

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado maior: nome, numeroRepetições, lista, resultado -> {V,F}
% Predicado para verificar qual o nome do estafeta aparece mais vezes numa lista
% Predicado auxiliar usado na query 1
maior(Nome, N, [], Nome).
maior(Nome, N, [H|T], R) :-
        quantosIguais([H|T], N1),
        N >= N1,
		maior(Nome, N, T, R), !.
maior(H, N1, [H|T], R) :-
        quantosIguais([H|T], N1),
		N < N1,
		maior(H, N1, T, R), !.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado todasEntregasDup: cliente, lista, resultado -> {V,F}
% Concatena todas as listas que do predicado estafetasEntregaCliente
% Predicado auxiliar usado na query 2
todasEntregasDup(C, [], []).
todasEntregasDup(C, [T|Tail], R) :-
        estafetasEntregaCliente(C, T, Temp),
	concatenar(Temp, Temp1, R),
	todasEntregasDup(C, Tail, Temp1).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado estafetasEntregaCliente: cliente, tipoEncomenda, estafeta -> {V,F}
% Devolve uma lista com os estafetas que entregaram um tipo de encomenda a um determinado cliente
estafetasEntregaCliente(C, T, R) :-
        findall(E, estafetasEntregaClienteValida(C, T, E), L),
	retiraDup(L, [], R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado estafetasEntregaClienteValida: cliente, tipoEncomenda, estafeta -> {V,F}
% Verifica se a entrega é valida e devolve o estafeta
estafetasEntregaClienteValida(C, T, E) :-
        entregaValida(E, V, KM, T, P, Pr, Vel, C, R, Cla, Denc, Dent).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado retiraDup: lista, _____, ______-> {V,F}
% Retira os duplicados presentes numa lista
% Predicado auxiliar usado nas queries 2 e 3
retiraDup([], [], nenhum).
retiraDup([H|T], [], R) :-
        retiraDup(T, [H], R), !.
retiraDup([H|T], A, R) :-
        member(H, A),
        retiraDup(T, A, R), !.
retiraDup([H|T], [HA|TA], R) :-
        retiraDup(T, [H, HA|TA], R), !.
retiraDup([], L, L).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado calculaValor: lista, ______ -> {V,F}
% Calcula o valor presente em cada triplo da lista e devolve o total
% Predicado auxiliar usado na query 4
calculaValor([], 0).
calculaValor([(V, P, KM)|T], R) :-
        custoTransporte(V, P, KM, Custo),
	calculaValor(T, Custo2),
	R1 is Custo + Custo2,
	round(R1, R, 1).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado custoTransporte: transporte, peso, km, custo -> {V,F}
% Custo de 2/Kg para transportar de bicicleta + 0.15/km
custoTransporte(bicicleta, P, KM, Custo) :-
        Custo is ((2 * P) + (0.20 * KM)).

% Custo de 4/Kg para transportar de bicicleta + 0.35/km
custoTransporte(mota, P, KM, Custo) :-
        Custo is ((4 * P) + (0.35 * KM)).

% Custo de 8/Kg para transportar de bicicleta + 0.50/km
custoTransporte(carro, P, KM, Custo) :-
        Custo is ((8 * P) + (0.50 * KM)).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado encontraValores: data, veiculo, peso, distancia -> {V,F}
% Devolve o veiculo, o peso e a distancia das entregas validas
% Predicado auxiliar usado na query 4
encontraValores(D/M/A/_, (V, P, KM)) :-
        entregaValida(E, V, KM, T, P, Pr, Vel, C, R, Cla, Denc, D/M/A/_).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado group: lista, listaAgregada -> {V,F}
% Agrupa elementos iguais de uma lista, contando as suas ocorrências
% Predicado auxiliar usado na query 5
group([], []).
group(List, Agg):-
        findall((Element,Size), (bagof(_,member(Element,List),Xs), length(Xs,Size)), Agg).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado calculaMedia: lista, resultado -> {V,F}
% Devolve a média de classificações atribuidas pelos clientes a um determinado estafeta
% Predicado auxiliar usado na query 6
calculaMedia(L, R) :-
        somatorio(L, S),
	length(L, C),
	C > 0,
	R1 is S / C, 
	round(R1, R, 1).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado round: valor, nrCasasDecimais, resultado -> {V,F}
% Arredonda para X para D casas decimais e devolve o resultado Y
round(X,Y,D) :-
        Z is X * 10^D,
        round(Z, ZA),
        Y is ZA / 10^D.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado veiculos: resultado -> {V,F}
% Coloca em R a lista de todos veículos
% Predicado auxiliar usado na query 7
veiculos(R) :-
        findall(V, entregaValida(E, V, KM, T, P, Pr, Vel, C, Zone, Class, Denc, Dent), L),
        retiraDup(L, [], R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado calculaVDifEntrega: data1, data2, lista1, lista2, nrEntregas -> {V,F}
% Função auxiliar que utiliza outra lista (N) para colocar os novos tuplos ((Veiculo,nº de entregas feitas pelo veículo no período))    
% Predicado auxiliar usado na query 7
calculaVDifEntrega(Data1,Data2,N,[Vult],Nentregas):-
        Nentregas = [(Vult,X)|N],
	calculaVentrega(Vult,X,Data1,Data2).

calculaVDifEntrega(Data1,Data2, N , [Vatual,Vprox|Outros] , Nentregas) :-
        Novo = [(Vatual,X)|N],
	calculaVentrega(Vatual,X,Data1,Data2),
        calculaVDifEntrega(Data1,Data2,Novo,[Vprox|Outros],Nentregas).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado calculaVentrega: veiculo, numero, data1, data2 -> {V,F}
% Função auxiliar que calcula o número total de entregas feitas por um veículo dentro do período D1 / D2    
calculaVentrega(Veiculo,N,D1,D2):-
        findall(_,(entregaValida(E, Veiculo, KM, T, P, Pr, Vel, C, Zone, Class, Denc, D),checkData(D1,D2,D)),L),
        length(L,N).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado quantosIguais: Lista,Comprimento -> {V,F}
% Conta quantos elementos iguais existem numa lista
% Predicado auxiliar usado nas queries 7 e 8
quantosIguais([], 0).
quantosIguais([H|T], N) :-
        member(H,T),
	quantosIguais(T,N1),
	N is N1 + 1.
quantosIguais([H|T], 1).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado estafetas: lista -> {V,F}
% Coloca em R a lista  de todas estafetas
% Predicado auxiliar usado na query 8
estafetas(R) :-
        findall(E, entregaValida(E, V, KM, T, P, Pr, Vel, C, Zone, Class, Denc, Dent), L),
        retiraDup(L, [], R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   - 
% Extensão do predicado calculaEstafEntregas: data1, data2, lista1, lista2, nrEntregas  -> {V,F}
% Função auxiliar que utiliza outra lista (N) para colocar os novos tuplos ((Estafeta,nº de entregas realizada no período))   
% Predicado auxiliar usado na query 8
calculaEstafEntregas(Data1,Data2,N,[Eult],Nentregas):-  
	Nentregas = [(Eult,X)|N],
	calculaEentrega(Eult,X,Data1,Data2).
											 
calculaEstafEntregas(Data1,Data2, N , [Eatual,Eprox|Outros] , Nentregas) :- 
	Novo = [(Eatual,X)|N],
	calculaEentrega(Eatual,X,Data1,Data2),
	calculaEstafEntregas(Data1,Data2,Novo,[Eprox|Outros],Nentregas).


%--------------------------------- - - - - - - - - - -  -  -  -  -   - 
% Extensão do predicado verificaPeriodo: data1, data2, resultado  -> {V,F}
% Função auxiliar que verifica a entrega está dentro do período estipulado
% Predcado auxiliar usado na query 9
verificaPeriodo(D1, D2, (P, DEnc, DEnt)) :-
        entrega(_,_,_,_,_,P,_,_,_,_,DEnc, DEnt),checkData(D1, D2, DEnt).


%--------------------------------- - - - - - - - - - -  -  -  -  -   - 
% Extensão do predicado calculaHoras: data, resultado  -> {V,F}
% Converte uma data para horas
calculaHoras(D/2/A/H,X) :-
        A mod 4 =:= 0,
        calculaHorasDia(D,W),
        calculaHorasDia(29,Y),
        calculaHorasDia(366,Z),
        X is (Y*2) + (Z*A) + W + H, !.
calculaHoras(D/2/A/H,X) :-
        calculaHorasDia(D,W),
        calculaHorasDia(28,Y),
        calculaHorasDia(365,Z),
        X is (Y*2) + (Z*A) + W + H, !.
calculaHoras(D/M/A/H,X) :-
        member(M, [1,3,5,7,8,10,12]),
        calculaHorasDia(D,W),
        calculaHorasDia(31,Y),
        calculaHorasDia(365,Z),
        X is (Y*M) + (Z*A) + H + W, !.
calculaHoras(D/M/A/H,X) :-
        calculaHorasDia(D,W),
        calculaHorasDia(30,Y),
        calculaHorasDia(365,Z),
        X is (Y*M) + (Z*A) + W + H, !.


%--------------------------------- - - - - - - - - - -  -  -  -  -   - 
% Extensão do predicado calculaHorasDia: dia, resultado  -> {V,F}
% Função auxiliar para calcular as horas totais de um dia do mês
calculaHorasDia(D,X) :-
        X is D * 24.


%--------------------------------- - - - - - - - - - -  -  -  -  -   - 
% Extensão do predicado periodoEmHoras: data1, data2, resultado  -> {V,F}
% Função auxiliar para calcular a diferença de horas entre duas datas
periodoEmHoras((DEnc,DEnt),X) :-
        calculaHoras(DEnt,Tent),
        calculaHoras(DEnc,Tenc),
        X is Tent - Tenc.


%--------------------------------- - - - - - - - - - -  -  -  -  -   - 
% Extensão do predicado foiEntregue: prazoEntrega, tempoEntrega, totalEntregue, totalNentregue -> {V,F}
% Função auxiliar para verificar quais  entregas passaram do prazo e quais foram entregue a tempo
foiEntregue((P,P2),Tent,Nent) :-
        P2 > P ,
        Tent is 0 ,
        Nent is 1,!.
foiEntregue(_,Tent,Nent) :-
        Tent is 1 , Nent is 0.


%--------------------------------- - - - - - - - - - -  -  -  -  -   - 
% Extensão do predicado contaEncomendas: lista, accEntregues, accNentregues, entregues, nEntregues -> {V,F}
% Função auxiliar para contabilizar o número total de entregas e não entregas
contaEncomendas([], Ent, NEnt, Ent, NEnt).
contaEncomendas([(E,_)|T], AcEnt, AcNEnt, Ent, NEnt) :-
        E =:= 1,
        NewEnt is AcEnt + 1,
        contaEncomendas(T,NewEnt,AcNEnt,Ent,NEnt), !.
contaEncomendas([_|T], AcEnt, AcNEnt, Ent, NEnt) :-
        NewNEnt is AcNEnt + 1,
        contaEncomendas(T,AcEnt,NewNEnt,Ent,NEnt).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado calculaEentrega: estafeta, numero, data1, data2 -> {V,F}
% Função auxiliar que calcula o número total de entregas feitas por uma estafeta dentro do período D1 / D2    
calculaEentrega(E,N,D1,D2):-
        findall(_,(entregaValida(E, V, KM, T, P, Pr, Vel, C, Zone, Class, Denc, D),checkData(D1,D2,D)),L),
        length(L,N).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado pesoTransEstafeta: data, estafeta, peso -> {V,F}
% Devolve um tuplo com o estafeta e o peso transportado num determinado dia
% Predicado auxiliar usado na query 10
pesoTransEstafeta(D/M/A/_, (E, P)) :-
        entregaValida(E, V, KM, T, P, Pr, Vel, C, Zone, Class, Denc, D/M/A/_).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado agrupa: lista1, lista2, resultado -> {V,F}
% Agrupa em tuplos os estafetas repetidos, somando os valores transportados
% Predicado auxiliar usado na query 10
agrupa([], R, R).
agrupa([H|T], [], R) :-
        agrupa(T, [H], R), !.
agrupa([H1|T1], [H2|T2], R) :-
        pertenceC(H1, [H2|T2]),
	atualizaPesos(H1, [H2|T2], A),
	agrupa(T1, A, R), !.
agrupa([H1|T1], [H2|T2], R) :-
        agrupa(T1, [H1, H2|T2], R), !.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado atualizaPesos: nome, peso, lista1, lista2 -> {V,F}
% Soma os valores transportados se os estafetas presentes nos tuplos forem iguais
atualizaPesos(X, [], []).
atualizaPesos((Nome1, Peso1), [(Nome1, Peso2)|T2], [(Nome1, PesoTotal)|A]) :- 
        PesoTotal is Peso1 + Peso2,
	atualizaPesos((Nome1, Peso1), T2, A), !.
atualizaPesos((Nome1, Peso1), [(Nome2, Peso2)|T2], [(Nome2, Peso2)|A]) :-
        Nome1 \= Nome2,
	atualizaPesos((Nome1, Peso1), T2, A), !.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado pertenceC: tupo, lista -> {V,F}
% 
pertenceC( (X, P),[(X, N)|L] ).
pertenceC( (X, P),[(Y, N)|L] ) :-
        X \= Y,
        pertenceC( (X, P),L ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado somatorio: Lista,Somatorio -> {V,F}
% Calcula o somatório de uma lista
somatorio([], 0).
somatorio([H|T], R) :-
        somatorio(T, R2),
	R is H + R2.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado concatenar: Lista1,Lista2,Resultado -> {V,F}
% Concatena duas listas
concatenar(L1, [], L1).
concatenar([], L2, L2).
concatenar([H1|T1], [H2|T2], [H1|L]) :-
        concatenar(T1, [H2|T2], L).


%-----------------------------------------------
% Extensão do predicado checkData: Data1,Data2,Data -> {V,F}.
% Verifica se uma data pertence a um período de tempo
checkData( D1/M1/A1/H1 , D1/M1/A1/H2 , D1/M1/A1/H ):- !,H >= H1,!,H =< H2, !.

checkData( _ , D2/M1/A1/H2 , D2/M1/A1/H ):- !,H =< H2, !.
checkData( D1/M1/A1/H1 , _ , D1/M1/A1/H ):- !,H >= H1, !.

checkData( D1/M1/A1/_ , D2/M1/A1/_ , D/M1/A1/_ ):- !,D >= D1,!,D =< D2, !.

checkData( _ , D2/M1/A1/_ , D/M1/A1/_ ):- !,D =< D2, !.
checkData( D1/M1/A1/_ , _ , D/M1/A1/_ ):- !,D >= D1, !.

checkData( _/M1/A1/_ , _/M2/A1/_ , _/M/A1/_ ):- !,M >= M1,!,M =< M2, !.

checkData( _ , _/M2/A1/_ , _/M/A1/_ ):- !,M =< M2, !.
checkData( _/M1/A1/_ , _ , _/M/A1/_ ):- !,M >= M1, !.

checkData( _/_/A1/_ , _/_/A2/_ , _/_/A/_ ):- !,A >= A1,!,A =< A2, !. 


%-----------------------------------------------
% Extensão do predicado checkPeriodo: Data1,Data2 -> {V,F}
% Verifica se o periodo fornecido é válido
checkPeriodo(D1/M1/A1/H1 , D1/M1/A1/H2) :- !,H1 =< H2, !.
checkPeriodo(D1/M1/A1/_ , D2/M1/A1/_) :- !,D1 < D2, !.
checkPeriodo(_/M1/A1/_ , _/M2/A1/_) :- !,M1 < M2, !.
checkPeriodo(_/_/A1/_ , _/_/A2/_) :- !,A1 < A2, !.