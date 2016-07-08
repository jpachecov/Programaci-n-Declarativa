% OPERADORES
% interseccion, union, union exclusiva y diferencia
% respectivamente.

:- op(600,yfx,:*:).  
:- op(600,yfx,:+:).
:- op(600,yfx,:^:).
:- op(600,yfx,:-:).
:- op(900,yfx,:=:).

% Ejercicio 3

interseccion([],_,[]).
interseccion([X|L],M,[X|L1]) :- pertenece(X,M),!, interseccion(L,M,L1).
interseccion([_|L],M,L1):- interseccion(L,M,L1).

union([],M,M).
union([X|L],M,K) :- pertenece(X,M),!, union(L,M,K).
union([X|L],M,[X|K]) :- union(L,M,K).

diferencia(A,[],A).
diferencia(L,[X|K],M):- pertenece(X,L), quita(X,L,L1), diferencia(L1,K,M),!.
diferencia(L,[_|K],M):- diferencia(L,K,M).

union_exclusiva(A,B,R) :- union(A,B,U), interseccion(A,B,I), diferencia(U,I,R).


% Evalua operaciones entre conjuntos
% representados por listas.
% Suponemos que las listas o conjuntos que
% nos dan esten bien formados (que sean conjuntos).
% El funtor p hace de parentesis, por lo que expresiones
% como la siguiente son validas:
% [2,3] :+: p(p([5,6,7] :^: [7,8,9]) :*: [1,5]) equivale a: {2,3} u (({5,6,7} ^ {7,8,9}) n {7,8,9})

eval([X|Y],[X|Y]).

eval(p(E) :+: p(F), R) :- eval(E :+: F,R).
eval(p(E) :+: F, R) :- eval(E :+: F,R).
eval(E :+: p(F), R) :- eval(E :+: F,R).

eval(p(E) :*: p(F), R) :- eval(E :*: F,R).
eval(p(E) :*: F, R) :- eval(E :*: F,R).
eval(E :*: p(F), R) :- eval(E :*: F,R).

eval(p(E) :-: p(F), R) :- eval(E :-: F,R).
eval(p(E) :-: F, R) :- eval(E :-: F,R).
eval(E :-: p(F), R) :- eval(E :-: F,R).

eval(p(E) :^: p(F), R) :- eval(E :^: F,R).
eval(p(E) :^: F, R) :- eval(E :^: F,R).
eval(E :^: p(F), R) :- eval(E :^: F,R).

eval(E :+: F, R) :- eval(E,E1), eval(F,F1), union(E1,F1,R).
eval(E :*: F, R) :- eval(E,E1), eval(F,F1), interseccion(E1,F1,R).
eval(E :-: F, R) :- eval(E,E1), eval(F,F1), diferencia(E1,F1,R).
eval(E :^: F, R) :- eval(E,E1), eval(F,F1), union_exclusiva(E1,F1,R).


% Igual que en el ejercicio 2.

E :=: R :- eval(E,R),!.


% Predicados auxiliares

concatenar([], L, L).
concatenar([X|L1], L2, [X|L]) :- concatenar(L1, L2, L).

% Predicado que se satisface cuanco X pertenece al conjunto
% (lista).

pertenece(X,[X|_]).
pertenece(X,[_|L]) :- pertenece(X,L).

% Predicado quita un elemento de un conjunto y devuelve
% el conjunto resultante.

quita(X,[X|L],L).
quita(X,[Y|L],K):- quita(X,L,L1),!,	 concatenar([Y],L1,K).



