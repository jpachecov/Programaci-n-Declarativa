% arista x,y 
% x vertice de inicio
% y vertice final
arista(a,b).
arista(b,c).
arista(c,d).
arista(b,d).
arista(a,d).
arista(a,e).
arista(e,d).
arista(d,f).
arista(f,e).

vertice(X):-arista(X,_).
vertice(X):-arista(_,X).

caminolongitud2(X,Y) :- arista(X,Z) , arista(Z,Y).

caminolongitudN(X,Y,1) :- arista(X,Y).
caminolongitudN(X,Y,N) :- N1 is N-1, 
						caminolongitudN(X,Z,N1),
						arista(Z,Y).

segmento(punto(X1,X2),punto(X2,Y2)).

triangulo(punto(X1,X2),punto(X2,Y2),punto(X3,Y3)).

cuadrado(P1,P2,P3,P4) :- validaPunto(P1).

validaPunto(punto(X,Y)).

vertical(segmento(X1,Y1), punto(X2,Y2)):- X1 = X2.

horizontal(segmento(X1,Y1), punto(X2,Y2)):- Y1 = Y2.


% el cuadra esta dado por sus cordenados en sentido horario

regular(C):- 
			C=cuadrado(P1,P2,P3,P4), 
		    vertical(segmento(P1,P2)),
		    vertical(segmento(P3,P4)).	

regular(C):- 
			C=cuadrado(P1,P2,P3,P4), 
		    horizontal(segmento(P1,P2)),
		    horizontal(segmento(P3,P4)).	

listaVertices([V1]) :- vertice(V1).
listaVertices([V1 | L]) :- vertice(V1) , listaVertices(L).
