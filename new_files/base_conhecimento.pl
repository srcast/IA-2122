%-------------------------------------------------------------------------
%---------------        Base de conhecimento         	------------------
%-------------------------------------------------------------------------

:-dynamic estafetaRanking/2. 

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado entrega: estafeta, veiculo, distancia, tipoEncomenda, pesoEnc, prazo, velocidade, cliente, rua, classificacao, dataEncomenda, dataEntrega -> {V, F}
entrega(manuel, bicicleta, 12, comida, 2, 1, 10, maria, 'avenida da liberdade, braga', 4, 11/10/2021/10, 11/10/2021/11).
entrega(manuel, bicicleta, 12, comida, 2, 1, 10, maria, 'avenida da liberdade, braga', 4, 11/10/2021/13, 11/10/2021/14).
entrega(manuel, bicicleta, 12, comida, 2, 1, 10, maria, 'avenida da liberdade, braga', 4, 11/10/2021/16, 11/10/2021/18).
entrega(manuel, bicicleta, 12, comida, 2, 1, 10, maria, 'avenida da liberdade, braga', 4, 11/10/2021/12, 11/10/2021/13).
entrega(jose, bicicleta, 15, comida, 4, 1, 10, ana, 'rua direita, barcelos', 5, 11/10/2021/12, 11/10/2021/13).
entrega(fabio, mota, 20, roupa, 12, 24, 35, filipa, 'rua de campelo, guimaraes', 5, 12/10/2021/13, 12/10/2021/19).
entrega(marco, carro, 23, movel, 80, 48, 25, cristina, 'travessa da igreja, famalicao', 3, 11/10/2021/17, 14/10/2021/10).
entrega(jose, bicicleta, 15, comida, 4, 1, 10, ana, 'rua direita, barcelos', 5, 11/10/2021/20, 11/10/2021/21).
entrega(manuel, bicicleta, 15, comida, 1, 1, 10, ana, 'rua direita, barcelos', 5, 11/10/2021/19, 11/10/2021/20).
entrega(fabio, bicicleta, 15, comida, 2, 1, 10, ana, 'rua direita, barcelos', 3, 12/10/2021/21, 12/10/2021/22).
entrega(manuel, bicicleta, 15, comida, 1, 1, 10, ana, 'rua direita, barcelos', 5, 11/10/2021/13, 11/10/2021/15).
entrega(manuel, mota, 12, roupa, 10, 18, 35, maria, 'avenida da liberdade, braga', 3, 12/10/2021/11, 13/10/2021/10).
entrega(manuel, mota, 23, comida, 3, 0.5, 35, cristina, 'travessa da igreja, famalicao', 4, 12/10/2021/14, 12/10/2021/15).
entrega(manuel, carro, 20, movel, 74, 30, 25, filipa, 'rua de campelo, guimaraes', 5, 13/10/2021/14, 16/10/2021/10).
entrega(manuel, carro, 23, comida, 3, 0.5, 25, cristina, 'travessa da igreja, famalicao', 4, 15/10/2021/18, 15/10/2021/19).
entrega(jose, carro, 15, roupa, 17, 4, 25, ana, 'rua direita, barcelos', 2, 15/10/2021/18, 17/10/2021/10).
entrega(jose, mota, 12, comida, 2, 1, 35, maria, 'avenida da liberdade, braga', 3, 14/10/2021/11, 14/10/2021/12).
entrega(jose, mota, 20, comida, 3, 0.5, 35, filipa, 'rua de campelo, guimaraes', 1, 12/10/2021/10, 12/10/2021/12).
entrega(jose, carro, 20, movel, 74, 30, 25, filipa, 'rua de campelo, guimaraes', 5, 13/10/2021/9, 16/10/2021/10).
entrega(jose, carro, 15, movel, 60, 5, 25, ana, 'rua direita, barcelos', 3, 14/10/2021/15, 15/10/2021/10).
entrega(fabio, carro, 15, movel, 45, 17, 25, ana, 'rua direita, barcelos', 2, 15/10/2021/15, 18/10/2021/10).
entrega(fabio, mota, 12, roupa, 9, 5, 35, maria, 'avenida da liberdade, braga', 3, 12/10/2021/16, 14/10/2021/10).
entrega(fabio, mota, 20, comida, 4, 2, 35, filipa, 'rua de campelo, guimaraes', 1, 12/10/2021/21, 12/10/2021/22).
entrega(fabio, carro, 23, movel, 53, 25, 25, cristina, 'travessa da igreja, famalicao', 2, 13/10/2021/4, 15/10/2021/10).
entrega(fabio, bicicleta, 15, comida, 2, 1, 10, ana, 'rua direita, barcelos', 3, 12/10/2021/13, 12/10/2021/14).
entrega(marco, bicicleta, 15, roupa, 3, 1, 10, ana, 'rua direita, barcelos', 2, 14/10/2021/9, 14/10/2021/18).
entrega(marco, mota, 12, comida, 4, 0.5, 35, maria, 'avenida da liberdade, braga', 4, 12/10/2021/10, 12/10/2021/12).
entrega(marco, mota, 23, comida, 3, 0.5, 35, cristina, 'travessa da igreja, famalicao', 5, 12/10/2021/11, 12/10/2021/12).
entrega(marco, carro, 20, movel, 74, 30, 25, filipa, 'rua de campelo, guimaraes', 5, 13/10/2021/11, 16/10/2021/10).
entrega(marco, carro, 23, movel, 89, 23, 25, cristina, 'travessa da igreja, famalicao', 5, 15/10/2021/14, 19/10/2021/10).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado estafetaRanking: estafeta, ranking -> {V, F}
estafetaRanking(jose, 4.3).
estafetaRanking(marco, 4.1).
estafetaRanking(fabio, 3.8).
estafetaRanking(manuel, 4.5).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado morada: cliente, rua, distancia -> {V, F}
morada(maria, 'avenida da liberdade, braga', 12).
morada(ana, 'rua direita, barcelos', 15).
morada(filipa, 'rua de campelo, guimaraes', 20).
morada(cristina, 'travessa da igreja, famalicao', 23).




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