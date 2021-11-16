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
%	imediato -> 0h					0 a 5 estrelas							avenida da liberdae
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
% Extensao do predicado entrega: estafeta, veiculo, tipoEncomenda, pesoEnc, volEnc?, prazo, velocidade, cliente, classificacao, rua, dia.

entrega(manuel, bicicleta, comida, 2, ?, 1, 10, maria, 4, avenida da liberdade, 11/10/2021).
entrega(jose, bicicleta, comida, 4, ?, 1, 10, ana, 5, rua direita, 11/10/2021).
entrega(fabio, mota, roupa, 12, ?, 24, 35, filipa, 5, rua de campelo, 11/10/2021).
entrega(marco, carro, movel, 80, ?, 48, 25, cristina, 3, travessa da igreja, 11/10/2021).

entrega(manuel, bicicleta, comida, 1, ?, 1, 10, ana, 5, rua direita , 11/10/2021).
entrega(manuel, mota, roupa, 10, ?, 18, 35, maria, 3, avenida da liberdade, 12/10/2021).
entrega(manuel, mota, comida, 3, ?, 0.5, 35, cristina, 4, travessa da igreja, 12/10/2021).
entrega(manuel, carro, movel, 74, ?, 30, 25, filipa, 5, rua de campelo, 13/10/2021).
entrega(manuel, carro, comida, 3, ?, 0.5, 25, cristina, 4, travessa da igreja, 15/10/2021).
