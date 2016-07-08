
% Para poder dar una configuracion inicial

peones_(L,M):- buscaSolucion(L,S),!, concatenar([L],S,M).

% Para la configuracion inicial pedida

peones(M):- buscaSolucion([b,b,b,-,n,n,n],S),!, concatenar([[b,b,b,-,n,n,n]],S,M).

% Encargado de encontrar los pasos a seguir para solucionar el problema

buscaSolucion(L,[M1|M]):- generaMovimiento(L,M1), not(esFinal(M1)), buscaSolucion(M1,M).
buscaSolucion(L,[M]):- generaMovimiento(L,M), esFinal(M).

% Predicado que genera un movimiento.

generaMovimiento(L,M):- blancoD(L,M).
generaMovimiento(L,M):- blancoS(L,M).
generaMovimiento(L,M):- negroI(L,M).
generaMovimiento(L,M):- negroS(L,M).

% Para decir que configuraciones son finales

esFinal([n,n,n,-,b,b,b]).
esFinal([n,n,-,b,b]).
esFinal([n,-,b]).

% Aplica la regla de blanco a la derecha si es que puede aplicarse

blancoD([X|[Y|L]],[X|R]):-blancoD([Y|L],R),!.
blancoD([b|[-|L]],[-|[b|L]]).

% Aplica la regla de blanco salta a la derecha si es que puede aplicarse

blancoS([X|L],[X|R]):-blancoS(L,R),!.
blancoS([b|[n|[-|L]]],[-|[n|[b|L]]]).

% Aplica la regla de negro a la IZQUERDA si es que puede aplicarse.
% Es mas facil invertir la lista y mover el peon a la derecha.

negroI(L,N):- reversa(L,R), negroD(R,D), reversa(D,N).

% Aplica la regla de negro salta IZQUIERDA, por comodidad
% se usa la reversa.

negroS(L,N):- reversa(L,R), negroS_aux(R,S), reversa(S,N).

% Brinca el peon negro a la derecha

negroS_aux([X|L],[X|R]):-negroS_aux(L,R),!.
negroS_aux([n|[b|[-|L]]],[-|[b|[n|L]]]).


% Mueve un peon negro a la derecha

negroD([X|[Y|L]],[X|R]):-negroD([Y|L],R),!.
negroD([n|[-|L]],[-|[n|L]]).


% Obtiene la reversa de una lista

reversa([X|L],R):- reversa(L,S), concatenar(S,[X],R).
reversa([],[]).

% Concatena dos listas

concatenar([], L, L).
concatenar([X|L1], L2, [X|L]) :- concatenar(L1, L2, L).
