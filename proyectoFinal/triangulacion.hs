import Data.List
import Graphics.Gloss



-- Se representará al poligono como una lista de vertices ordenados
-- en sentido contrario a las manecillas del reloj.


data Poligono = Pol [Vertice] deriving Show
data Vertice = V [Char] Punto
data Punto = Punto Float Float deriving Show
data Arista = Arista Vertice Vertice

instance Show Vertice where
	show = muestraVertice
muestraVertice (V a p) = a 

instance Show Arista where
	show = muestraArista
muestraArista (Arista v1 v2) = "A(" ++ (show v1) ++ "," ++ (show v2) ++ ")"

instance Eq Vertice where
	(==) (V s1 p1) (V s2 p2) = (s1 == s2) && (p1 == p2)

instance Eq Punto where
	(==) (Punto a b) (Punto c d) = a == c && b == d

v1 = V "a" (Punto 0 5)
v2 = V "b" (Punto (-2) (4))
v3 = V "c" (Punto (-3) (2))
v4 = V "d" (Punto (-2) (1))
v5 = V "e" (Punto (-1) (-3))
v6 = V "f" (Punto (2) (-2))
v7 = V "g" (Punto (4) (0))
v8 = V "h" (Punto (3) (3))

vertices = [v1,v2,v3,v4,v5,v6,v7,v8] 
pol = Pol vertices


-- Triangula un poligono y-monotono y regresa una lista de
-- aristas, que representan las aristas que estan en la triagunlacion.
-- Cabe mencionar que en esta lista esta cada una de las aristas que forman
-- la orilla del poligono.

triangulaMonotono :: Poligono -> [Arista]
triangulaMonotono (Pol vertices) 
								| ((length vertices) == 3) = (obtenAristas (Pol vertices))
								| ((length vertices) < 3) = error "No se puede"
								| otherwise = triangulaMonotono_aux (Pol vertices)


-- Esta función obtiene la lista de aristas de un polígono
obtenAristas :: Poligono -> [Arista]
obtenAristas (Pol (x:xs)) = obtenAristas_aux x (x:xs)


-- Guarda en una lista las aristas y cuando llega al ultimo vertice
-- en la lista de vertices del pligono, crea la arista que va del ultimo al primero.

obtenAristas_aux primero (ultimo:[]) = [Arista ultimo primero]
obtenAristas_aux primero (x:xs) = [Arista x (head xs)] ++ (obtenAristas_aux primero (xs))


-- función que llama a la función triangulaM que simula el algoritmo iterativo 
-- para triangular un poligono y-monotono.
triangulaMonotono_aux (Pol vertices) = tringulaM (Pol vertices) (reverse (take 2 ordenados)) (drop 2 ordenados) ordenados where ordenados = quicksort vertices


-- 
tringulaM (Pol vertices) (c:pila) (v:vs) ordenados = 
			tringulaM_aux cadenas (c:pila) (v:vs) ordenados
			where cadenas = obtenCadenas (Pol vertices)

tringulaM_aux (i,d) (c:pila) [v] _ = agregaUltimasDiagonales v (c:pila)
tringulaM_aux (cizq,cder) (c:pila) (v:vs) ordenados =
	if (enCadenasDistintas (cizq,cder) c v)
		then 
			let newStack = 
				(let indice = elemIndex v ordenados 
					in case indice of (Just i) -> [((!!) ordenados (i-1)),v]) in
											    									--Nothing -> error "ups") in
			(generaTodasDiagonales v (c:pila)) ++ 
			(tringulaM_aux (cizq,cder) newStack vs ordenados)
			
		else
			if(elem v cizq)
				then 
				--de lado izquierdo
				-- Si esta a laizquierda paramos
				let (diagonales,newStack) = (agregaDiagonalesInternas "izquierda" v (c:pila)) in
					diagonales ++ (tringulaM_aux (cizq,cder) (v:newStack) vs ordenados)
				else 
				--de lado derecho
				--Si esta a la derecha paramos
				let (diagonales,newStack) = (agregaDiagonalesInternas "derecha" v (c:pila)) in
					diagonales ++ (tringulaM_aux (cizq,cder) (v:newStack) vs ordenados)


generaTodasDiagonales v [ultimo] = []
generaTodasDiagonales v (c:pila) = [Arista v c] ++ generaTodasDiagonales v pila

agregaDiagonalesInternas :: [Char] -> Vertice -> [Vertice] -> ([Arista],[Vertice])
agregaDiagonalesInternas "izquierda" v [v1] = ([],[v1])
agregaDiagonalesInternas "izquierda" v (c:pila) =
		if(esVueltaIzquierda v c (head pila))
			then ([],(c:pila))
			else let (diagonales,stack) = agregaDiagonalesInternas "izquierda" v pila
					in ([(Arista v (head pila))] ++ diagonales,stack)

agregaDiagonalesInternas "derecha" v [v1] = ([],[v1])
agregaDiagonalesInternas "derecha" v (c:pila) =
		if(esVueltaDerecha v c (head pila))
			then ([],(c:pila))
			else let (diagonales,stack) = agregaDiagonalesInternas "derecha" v pila
					in ([(Arista v (head pila))] ++ diagonales,stack)

agregaUltimasDiagonales v (c:pila) = generaTodasDiagonales v pila

enCadenasDistintas :: ([Vertice],[Vertice]) -> Vertice -> Vertice -> Bool
enCadenasDistintas (cizq,cder) (V s p) (V s1 p1) = 
		if ((elem (V s p) cizq) && (elem (V s1 p1) cder) || (elem (V s p) cder) && (elem (V s1 p1) cizq))
			then True
			else False			

obtenCadenas :: Poligono -> ([Vertice],[Vertice])
obtenCadenas (Pol vertices) = let ordenados = quicksort vertices in
							let primero = head ordenados in
								let ultimo = last ordenados in
									(obtenCadenaIzq_aux primero ultimo (Pol vertices),obtenCadenaDer_aux ultimo primero (Pol vertices))

-- Obtiene la cadena izquierda de una poligono, empezando desde el vertice
-- con la coordenada Y mayor.

obtenCadenaIzq_aux primero ultimo (Pol (x:xs)) = [primero] ++ (agregaHastaUltimo ultimo (encuentraVertice primero (Pol (x:xs)) (Pol (x:xs)))) (Pol (x:xs))

obtenCadenaDer_aux primero ultimo (Pol (x:xs)) = [primero] ++ (agregaHastaUltimo ultimo (encuentraVertice primero (Pol (x:xs)) (Pol (x:xs)))) (Pol (x:xs))


encuentraVertice v (Pol []) (Pol todos) = encuentraVertice v (Pol todos) (Pol [])
encuentraVertice v (Pol (v1:xs)) (Pol todos)
							| (v == v1) = (Pol xs)
							|otherwise = encuentraVertice v (Pol xs) (Pol todos)

agregaHastaUltimo v (Pol []) (Pol todos) = agregaHastaUltimo v (Pol todos) (Pol [])							
agregaHastaUltimo v (Pol (v1:xs)) (Pol todos)
							| (v == v1) = [v]
							| otherwise = [v1] ++ (agregaHastaUltimo v (Pol xs)) (Pol todos)
					



quicksort [] = []
quicksort (x:xs) = quicksort small ++ (x : quicksort large)
  where small = [y | y <- xs, (mayorEq y x)]
        large = [y | y <- xs, (menor y x)]

mayorEq :: Vertice -> Vertice -> Bool
mayorEq (V s (Punto a b)) (V s1 (Punto c d))
		| (b == d) = a >= c
		|otherwise = b >= d

-- prim > seg
menor :: Vertice -> Vertice -> Bool
menor (V s (Punto a b)) (V s1 (Punto c d))
		| (b == d) = a < c
		|otherwise = b < d

-- esVueltaIzquierda a b c es verdadero si y solo si
-- El vector c-a esta a la izquierda del vector de referencia b-a
esVueltaIzquierda :: Vertice -> Vertice -> Vertice -> Bool
esVueltaIzquierda (V s1 p1) (V s2 p2) (V s3 p3) = esVueltaIzquierda_aux p1 p2 p3

esVueltaIzquierda_aux (Punto a b) (Punto c d) (Punto e f) 
		| (p_punto (c-a,d-b) (e-a,f-b)) == 0 = False
		| (p_punto (c-a,d-b) (e-a,f-b)) < 0 = False
		| otherwise = True


-- esVueltaDerecha a b c es verdadero si y solo si
-- El vector c-a esta a la derecha del vector de referencia b-a
esVueltaDerecha :: Vertice -> Vertice -> Vertice -> Bool
esVueltaDerecha (V s1 p1) (V s2 p2) (V s3 p3) = esVueltaDerecha_aux p1 p2 p3


esVueltaDerecha_aux (Punto a b) (Punto c d) (Punto e f) 
		| (p_punto (c-a,d-b) (e-a,f-b)) == 0 = False
		| (p_punto (c-a,d-b) (e-a,f-b)) > 0 = False
		| otherwise = True

-- Producto vectorial
--p_punto A B = A x B
p_punto :: (Float,Float) -> (Float,Float) -> Float
p_punto (a,b) (c,d) = (a*d) - (b*c)


-- Funcion que obtiene sólo los puntos, parejas ordenas, de los vertices
-- que represntan al poligono.

daPuntos :: Poligono -> [(Float,Float)]
daPuntos (Pol []) = []
daPuntos (Pol ((V s (Punto a b)):vs)) = ((a,b):daPuntos_aux (V s (Punto a b))(Pol vs))
daPuntos_aux (V s (Punto x  y)) (Pol []) = [(x,y)]
daPuntos_aux p (Pol ((V s (Punto x y)):vs)) = ((x,y):daPuntos_aux p (Pol vs)) 

-- Funcion que obtiene los segmentos generados por 
-- ael algoritmo de triangulacion de un poligono y-monotono

obtenSegmentosT :: Poligono -> [Picture]
obtenSegmentosT (Pol vertices) = 
				let aristas = triangulaMonotono (Pol vertices)
				in let orilla = obtenAristas (Pol vertices)
					in (obtenLineas aristas) ++ (obtenLineas orilla)


obtenLineas :: [Arista] -> [Picture]
obtenLineas [] = []
obtenLineas ((Arista v1 v2):as) = [Line [(obtenPunto v1),(obtenPunto v2)]] ++ obtenLineas as

obtenPunto :: Vertice -> (Float,Float)
obtenPunto (V s (Punto a b)) = (a,b) 

main 	
 = display 
        (InWindow
	       "Triangulación" 	 -- window title
		(1000, 600) 	     -- window size
		(100, 100)) 	     -- window position
	white			         -- background color
	picture			         -- picture to display

picture	= 
	Pictures [pol1,pol2,titulo1,titulo2]
		where 
			pol1 = Translate (200) (0) (Scale 40 40 (Pictures (obtenSegmentosT pol)))
			pol2 = Translate (-200) 0 (Scale 40 40 (Pictures (obtenLineas (obtenAristas pol))))
		  	titulo1 = Translate (-300) (250) (Scale 0.2 0.2 (Text "Poligono y-monotono"))
		  	titulo2 = Translate (80) (250) (Scale 0.2 0.2 (Text "Poligono triangulado"))
		  