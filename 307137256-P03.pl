
% Ejercicio 1.

:- op(500,yfx,:+:).

t(0 :+: 1, 1 :+: 0).
t(X :+: 0 :+: 1,X :+: 1 :+: 0).
t(X :+: 1 :+: 1, Z):- t(X :+: 1,X1), t(X1 :+: 1,Z).



% Ejercicio 2.

:- op(400,yfx,:-:).
:- op(700,yfx,:*:).
:- op(600,yfx,:/:).
:- op(900,yfx,:=:).

% Genera un arbol usando t a partir de la expresion dada.
% EL funtor p hará de paréntesis, por lo que expresiones con 
% paréntesis son validas como entrada
% EJEMPLO:
% 5:+:p(8:*:p(4:/:2))   equivale a: 5 + (8 * (4 / 2))



generaArbol(p(E) :*: p(F),t(*,p(L),p(R))):- generaArbol(E,L),!, generaArbol(F,R).
generaArbol(E :*: p(F),t(*,L,p(R))):- generaArbol(E,L),!, generaArbol(F,R).
generaArbol(p(E) :*: F,t(*,p(L),R)):- generaArbol(E,L),!, generaArbol(F,R).

generaArbol(p(E) :+: p(F),t(+,p(L),p(R))):- generaArbol(E,L),!, generaArbol(F,R).
generaArbol(E :+: p(F),t(+,L,p(R))):- generaArbol(E,L),!, generaArbol(F,R).
generaArbol(p(E) :+: F,t(+,p(L),R)):- generaArbol(E,L),!, generaArbol(F,R).

generaArbol(p(E) :/: p(F), t(/,p(L),p(R))) :- generaArbol(E,L),!, generaArbol(F,R).
generaArbol(E :/: p(F), t(/,L,p(R))) :- generaArbol(E,L),!, generaArbol(F,R).
generaArbol(p(E) :/: F,t(/,p(L),R)):- generaArbol(E,L),!, generaArbol(F,R).

generaArbol(p(E) :-: p(F),t(-,L,R)):- generaArbol(E,L),!, generaArbol(F,R).
generaArbol(E :-: p(F),t(-,L,R)):- generaArbol(E,L),!, generaArbol(F,R).
generaArbol(p(E) :-: F,t(-,L,R)):- generaArbol(E,L),!, generaArbol(F,R).



generaArbol(E :*: F,t(*,L,R)):- generaArbol(E,L),!, generaArbol(F,R).
generaArbol(E :/: F,t(/,L,R)):- generaArbol(E,L),!, generaArbol(F,R).
generaArbol(E :+: F,t(+,L,R)):- generaArbol(E,L),!, generaArbol(F,R).
generaArbol(E :-: F,t(-,L,R)):- generaArbol(E,L),!, generaArbol(F,R).
generaArbol(N,t(N,~,~)).



% Recorre un arbol usando t en postorden

post(p(E),[p(R)]):- post(E,R),!.
post(t(+,L,R),P):- post(L,P1), !, post(R,P2), concatenar(P2,[+],P3), concatenar(P1,P3,P).
post(t(-,L,R),P):- post(L,P1),!, post(R,P2), concatenar(P2,[-],P3), concatenar(P1,P3,P).
post(t(*,L,R),P):- post(L,P1),!, post(R,P2), concatenar(P2,[*],P3), concatenar(P1,P3,P).
post(t(/,L,R),P):- post(L,P1),!, post(R,P2), concatenar(P2,[/],P3), concatenar(P1,P3,P).
post(t(N,~,~),[N]).


% Evalua 
eval(p(X),R):- eval(X,R),!.
eval([error|_],error).
eval([_,0,/],error).
eval([A,B,*],R):- eval(A,A1),!, eval(B,B1) , R is A1 * B1.
eval([A,B,/],R):- eval(A,A1),!, eval(B,B1),  R is A1 / B1.
eval([A,B,+],R):- eval(A,A1),!,eval(B,B1) , R is A1 + B1.
eval([A,B,-],R):- eval(A,A1),!,eval(B,B1), R is A1 - B1.
eval(L,R):-obten(3,L,L3,R2), eval(L3,R1),!, eval([R1|R2],R).
% EL ultimo caso es cuando X es numero
eval([X],X):-!.
eval(X,X).

% Evalua una expresion que esta dada usando los
% operadores definidos.

evalExpresion(E,R):- generaArbol(E,T), post(T,P), eval(P,R),!.

E :=: R :- evalExpresion(E,R).



% PREDICADOS AUXILIARES

% Obtiene los primeros N elementos de [X|L] y los pone en [X|K] dejando
% en R una copia del resto de la lista original al quitarle N elementos

obten(N,[X|L],[X|K],R):- M is N - 1, obten(M,L,K,R).
obten(0,R,[],R).  

% Concatena dos listas

concatenar([], L, L).
concatenar([X|L1], L2, [X|L]) :- concatenar(L1, L2, L).

