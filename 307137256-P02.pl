% Practica 02.
% Carlos Augusto Escalona Navarro 308264113
% Jean Pierre Pacheco Avila 307137256

%
%EJERCICIO 3
%

%
% Predica que niega a P
%

not_1(P, R):- P=1, !,R is 0.
not_1(P,R):- R is 1.

%
%Predicado que hace la conjuncion de P1 y P2 y regresa el resultado en R
%

and_1(P1, P2, R):- P1=1, P2=1,!, R is 1.
and_1(P1, P2, R):- P1=0,!,  R is 0.
and_1(P1, P2, R):- P2=0,  !, R is 0.

%
%Predicados auxiliares
%
niega_cabeza([L|LS],R):- not_1(L,RS), concatenar([RS],LS,L1), R = L1.
cabeza_and([L|LS],R):-L=and,  R = LS.
pertenece(X,[X|_]):-!.
pertenece(X,[_|L]):-pertenece(X,L).




%
%Predica que aplica el not a una lista , aplica el predicado not al elemento
%que esta inmediatamente despues de este. La lista que recibe debe estar
%bien formada este predicado no revisa si la lista esta bien formada

evalua_not([], []).
evalua_not([not|LS], [C|XL]):- niega_cabeza(LS,[C|L]),!,evalua_not(L,XL).
evalua_not([L|LS], [L|XS]):- evalua_not(LS,XS).



%
%Predica que aplica el and a una lista y nos regresa el valor booleano de esta lista  ie
% nos regresa 1 si todos los elementos de los ands son 1 y nos regresa 0 si existe almenos un 0
% dentro de la lista que aplicara el and, aplica el predicado and al elemento
%que esta inmediatamente despues de este con el que esta antes del and de la lista.
% La lista que recibe debe estar bien formada este predicado no revisa si la lista esta bien formada.

evalua_and([], Z):-!.
evalua_and(L, N):-pertenece(0,L), !,N = 0.
evalua_and(L, N):-  N = 1.




%
%Predica que evalua a una lista , aplica el predicado evalua_not  primero y
%luego aplica el predicado evalua_and. La lista que recibe debe estar
%bien formada este predicado no revisa si la lista esta bien formada

evalua([],R):-!.
evalua(L,R):-evalua_not(L,Z),evalua_and(Z,Y), R=Y.





% Predicado para obtener las coordenadas para representar
% un arbol binario en una retícula.
% Recibe un bin()  y hace uso de los predicados 'asigna_y/2' y 'asigna_x/3' además
% del predicaado 'inorden/2' que nos proporcionaron.

dibuja_arbol_binario(vacio,_):-!.
dibuja_arbol_binario(bin(H1,C,H2),R):-
	asigna_y(bin(H1,C,H2),S),
	inorden(bin(H1,C,H2),L),
	asigna_x(S,L,R).


% Obtiene a partir de un arbol binario en forma de bin()
% un arbol equivalente en forma de ab() en donde la coordenada Y
% ya esta correctamente determinada para cada nodo.
% A la coordenada X le asigna -1.
% Esto se hace descendiendo por el arbol, en inorden por ejemplo.


asigna_y(bin(A,B,C),R):- recorre_inorden(bin(A,B,C),1,R).


% [recorre_inorden] Deciende por el arbol asignando la coordenada en Y.

recorre_inorden(vacio,_,vacio).
recorre_inorden(bin(H1,C,H2),N, ab(C,-1,N,R,A)):-
	N1 is N + 1,
	recorre_inorden(H1,N1,R),
	recorre_inorden(H2,N1,A).


% Obtiene el indice S de un elemento X en la lista L, si el
% primer elemento esta en R. indice_x(X,L,R,S).
% Devuelve -1 si no esta.

indice_x(_,[],_,-1):-!.
indice_x(X,[X|_],S,S):-!.
indice_x(X,[_|L],R,S):- R1 is R +1, indice_x(X,L,R1,S).


% A partir de un arbol representado con el funtor ab,
% obtiene un arbol equivalente usando ab pero
% la coordenada en X ya esta determinada por el
% recorrido en inorden sobre el arbol.

asigna_x(vacio,_,vacio).
asigna_x(ab(W,_,Y,H1,H2),L,ab(W,R,Y,H3,H4)):- indice_x(W,L,1,R),asigna_x(H1,L,H3),asigna_x(H2,L,H4).


% Concatena dos listas

concatenar([], L, L).
concatenar([X|L1], L2, [X|L]) :- concatenar(L1, L2, L).

% Recorre en inorden el arbol binario
% generando una lista con los vertices en orden
% del recorrido.

inorden(vacio, []) :- !.
inorden(T, L) :-
	T = bin(I,N, D),
	inorden(I, L1),
	inorden(D, L2),
	concatenar(L1, [N], R),
	concatenar(R, L2, L).



% Predicado para obtener el máximo común divisor de
% dos números naturales mayores que cero.
% mcd(A,B,X) = X es el maximo comun divisor de A y B.

mcd(0,_,indefinido):-!.
mcd(_,0,indefinido):-!.
mcd(1,_,1):- !.
mcd(_,1,1):- !.
mcd(A,A,A):- !.

mcd(A,B,X):- A < B, C is mod(B,A), mcd_aux(A,B,C,X),!.
mcd(A,B,X):- B < A, C is mod(A,B), mcd_aux(A,B,C,X).

% Predicado que hace uso del ultimo residuo calculado
% Para obtener el maximo común divisor, ya que por el
% algoritmo extendido de Euclides sabemos que el ultimo residuo
% distinto de cero es el máximo común divisor.

mcd_aux(_,B,C,B):- C = 0,!.
mcd_aux(A,_,C,X):- mcd(A,C,X).


% Hace lo mismo pero usando el predicado modulo/3 definido
% mas abajo.

mcd2(0,_,indefinido):-!.
mcd2(_,0,indefinido):-!.
mcd2(1,_,1):- !.
mcd2(_,1,1):- !.
mcd2(A,A,A):- !.

mcd2(A,B,X):- A < B, modulo(A,B,C), mcd_aux2(A,B,C,X),!.
mcd2(A,B,X):- B < A, modulo(B,A,C), mcd_aux2(A,B,C,X).

% Igual que mcd_aux.

mcd_aux2(_,B,C,B):- C = 0,!.
mcd_aux2(A,_,C,X):- mcd2(A,C,X).


% Predicado para calcular modulo.
% R es el residuo resultante de dividir el número
% mayor entre el menor.
% Suponemos que A < B

modulo(1,_,0).
modulo(_,1,0).
modulo(0,A,A).
modulo(_,0,errordiv).
modulo(A,B,R):- calculaModulo(A,B,2,R),!.

% Suponemos que A < B, y A B naturales.
% Este predicado obtiene R tal que:
% B = NA + R , 0 <= R < A, N natural

calculaModulo(A,B,N,R):- Q is A*N, S is N - 1, Q > B, R is B - (S*A),!.
calculaModulo(A,B,N,0):- Q is A*N, Q = B.
calculaModulo(A,B,N,R):- Q is A*N, Q < B, K is N + 1, calculaModulo(A,B,K,R).






