:-dynamic oi/1.

oi(1).

altera(X):- findall(Y, (oi(Y), Y < X), L), altera2(L,X).
altera2([],X):- asserta(oi(X)).
altera2([H|T],X):- oi(H), retractall(oi(H)),altera2(T,X).
altera2([_|T],X):- altera2(T,X).

slave(5).







