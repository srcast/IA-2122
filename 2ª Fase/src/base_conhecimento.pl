%-------------------------------------------------------------------------
%---------------        Base de conhecimento         	------------------
%-------------------------------------------------------------------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Definição das arestas do grafo
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
move(trandeiras,real,2).
move(real,saoVicente,5).
move(real,greenDistribution,8).
move(saoVicente,pedralva,6).
move(saoVicente,tenoes,4).
move(pedralva,priscos,7).
move(tenoes,cividade,2).
move(tenoes,priscos,1).
move(cividade,greenDistribution,3).
move(padimDaGraca,crespos,2).
move(padimDaGraca,ferreiros,3).
move(crespos,tadim,7).
move(ferreiros,tadim,5).
move(ferreiros,espinho,1).
move(tadim,nogueira,3).
move(tadim,greenDistribution,4).
move(espinho,nogueira,1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Estimativa da distância em linha reta de um nodo até à Green Distribution
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
% Definição das encomendas
% Extensão do predicado entrega: estafeta, tipoEncomenda, pesoEnc, prazo,cliente, rua, classificacao, dataEncomenda, dataEntrega -> {V, F}
entrega(manuel, comida, 2, 1, maria, 'crespos', 4, 11/10/2021/10, 11/10/2021/11).
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
% Determina um índice numérico para cada veículo
veiculoIndice(bicicleta,1).
veiculoIndice(mota,2).
veiculoIndice(carro,3).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Definiçãp do estado inicial
inicial(greenDistribution).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Definição do estado final
final(inicial(greenDistribution)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado objetivo
objetivo(greenDistribution).