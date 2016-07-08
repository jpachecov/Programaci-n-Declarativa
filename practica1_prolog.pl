


reversa([],[]).
reversa([X|Y],Z):- concatenar(S,[X],Z), reversa(Y,S).


concatenar([],L,L).
concatenar([X|M],L,[X|Z]):-concatenar(M,L,Z).

pertenece(X,[X|_]):- !.
pertenece(X,[_|L]):- pertenece(X,L).




a(B,B).


$ la suma del polinomio vacio [] con un polinomio P pues es P. 
$ LA suma de un polinomio [(X,Y) | L1] con P es igual a la 
$ suma de (X,Y) + P = R y luego a R sumarle el restante L1.

suma_polinomios([],P,P).
suma_polinomios( [ (X,Y) | L1 ] ,P, Q):- sumaTermino((X,Y),P,R),suma_polinomios(L1,R,Q).

$ este predicado calcula la suma de un solo termino con un polinomio.
$ estoy suponiendo que en [(Z,Y)|P] nohay dos o mas elementos de la forma (x,y)
$ es decir si esta (2,3) entonces noesta (x,3) para toda x.
$ esto es porque si nohay ningun termino en [(Z,Y)|P] contra quien pueda sumar (x,Y) entonces
$ debo agregar (X,Y) al polinomio resultante: [(Z,Y)|P]+[(X,Y)]
$ Pero si sí hubo alguno contra quien sí lo sumé, entonces ya no debo agregar a
$ (X,Y) al polinomio resultante.
$ por eso hay dos funciones, sumaTerminoSi es llamada cuando si pude sumar el término.

sumaTermino((X,Y),[(Z,Y)|P],[(W,Y)|L2]):- W is X+Z, sumaTerminoSi((X,Y),P,L2).
sumaTermino((X,Y),[(Z,W)|P],[(Z,W)|L]):-sumaTermino((X,Y),P,L).
sumaTermino((X,Y),[],[(X,Y)]). 

sumaTerminoSi((X,Y),[(Z,W)|P],[(Z,W)|L]):-sumaTerminoSi((X,Y),P,L).
sumaTerminoSi((X,Y),[],[]). 

$ Calcula el conjunto potencia, aun no funciona


$potencia([],[[]]).
potencia([X|[]],[[X]]).
$potencia([X|Y],[[X]|Z]):-potencia(Y,Z), potencia().

$potencia([],[[]]).
potencia([X|L], [Z|P]):- creaConjuntos(X,L,Z1), concatenar([[X|L]],Z1,Z) , potencia1(L,P).

potencia1([X|[]],L):-concatenar([[]],[[X]],L).
$potencia1([X|[]],[[X]]).
potencia1([X|L], [Z|P]):- creaConjuntos(X,L,Z), potencia1(L,P).

creaConjuntos(X,[],[[X]]).
creaConjuntos(X,[Y|L],[Z|L1]):- concatenar([X],[Y],Z), creaConjuntos(X,L,L1).

