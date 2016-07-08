

----- Ejercicio 1 -----




divisores::Int -> [Int]

divisores 0 = []
divisores n | n > 0 = agregaInversos (foldr (\x y-> agrega_divisor y x n) [1,n] [2..(n-1)])
			| otherwise = agregaInversos (foldr (\x y-> agrega_divisor y x n) [1,-n] [2..(-n-1)])


-- Agrega y a la lista xs si y es divisor de n

agrega_divisor xs y n | (mod n y) == 0 = xs ++ [y] 
				 | otherwise = xs

-- Esta funcion regresa xs más los inversos aditivos de los elementos en xs

agregaInversos xs = xs ++ (foldr (\x y -> y ++ [-x]) [] xs)

mayorRaizEntera:: [Int] -> Int
mayorRaizEntera (a0:as) = obtenMayor (encuentraRaices posibles_raices (a0:as)) where posibles_raices = divisores a0


encuentraRaices posibles_raices coeficientes = foldr (\r rs -> agregaRaiz r rs coeficientes) [] posibles_raices

agregaRaiz r rs coeficientes = if (evaluacion r coeficientes) == 0 then rs ++ [r] else rs


-- Funcion que obtiene la evaluacion del polinomio
-- represenatado como la lista de sus coeficientes y un numero x
-- Usamos una tupla para poder obtener la evaluacion usando foldr ---> (suma, exponente)

evaluacion x coeficientes = fst ( foldr (\ a y -> (fst(y) + a*(x^snd(y)), snd(y) + 1)) (0,0) (reversa coeficientes))


-- Obtiene la reversa de una lista cualquiera

reversa (x:xs) = foldr (\ x y -> y ++ [x]) [] (x:xs)

-- Obtiene la raiz mayor de una lista de raices
-- Se manejó el caso de error aqui por comodidad

obtenMayor [] = error "El polinomio no tiene raices enteras"
obtenMayor (x:xs) = foldr (\x acc -> if(x > acc) then x else acc) x xs



----- Ejercicio 2 -----

paraTodo :: [a] -> (a -> Bool) -> Bool
paraTodo (x:xs) f = foldl (\acc elem -> acc && (f elem)) True (x:xs)

existeAlgun :: [a] -> (a -> Bool) -> Bool
existeAlgun (x:xs) f = existe_aux f False (x:xs)

-- Para parar cuando antes la evaluacion
existe_aux f True _ = True
existe_aux f _ (x:xs) = existe_aux f (f x) xs
existe_aux f _ _ = False


----- Ejercicio 3 -----
-- [x | xs <- xss; x <- xs; odd x]
-- Es equivalente a :

obtenImpares xss = filter odd (concat xss)


-- [ (x, y) | x <− xs; p x; y <− ys ]
-- Es equivalente a :

funcion xs ys p = concat (map (\ x -> map (\y -> (x,y)) ys) (filter p xs))



----- Ejercicio 4 -----

aplicaListaFunc funciones x = map (\ f -> f x) funciones


----- Ejercicio 5 -----

data Entero = Cero | S Entero | P Entero

instance Show Entero where
	show = muestraEntero

muestraEntero :: Entero -> String
muestraEntero n = let a = simplifica n 
	in case a of 
		Cero -> "0"
		(S x1) -> "S " ++ (muestraEntero2 x1)
		(P x1) -> "P " ++ (muestraEntero2 x1)
	where
		muestraEntero2 Cero = "Cero"
		muestraEntero2 (S n) = "S " ++ muestraEntero2 n
		muestraEntero2 (P n) = "P " ++ muestraEntero2 n

instance Eq Entero where
	(==) = comparaEnteros

comparaEnteros :: Entero -> Entero -> Bool
comparaEnteros n1 n2 = comparaAux (simplifica n1) (simplifica n2)

comparaAux :: Entero -> Entero -> Bool
comparaAux Cero Cero = True
comparaAux (S x) (S y) = comparaAux x y
comparaAux (P x) (P y) = comparaAux x y
comparaAux _ _ = False

simplifica :: Entero -> Entero
simplifica Cero = Cero
simplifica (S (S n)) = S (simplifica  (S n))
simplifica (P (P n)) = P (simplifica (P n)) 
simplifica (S (P n)) = simplifica n
simplifica (P (S n)) = simplifica n
simplifica n = n

sumaEnteros :: [Entero] -> Entero
--foldr elem acc
sumaEnteros (x:xs) = foldr (\x acc -> sumaEntero acc x) Cero (x:xs)



sumaEntero :: Entero -> Entero -> Entero

sumaEntero Cero y = y
sumaEntero (S x) y = S (sumaEntero x y)
sumaEntero (P x) y = P (sumaEntero x y)


----- Ejercicio 6 -----
 
-- (a) --

-- Calcula los segmentos
-- Si la longitud de la cadena es n, entonces
-- el numero de segmentos es: (n(n+1)/2) + 1

segs [] = [[]]	
segs (x:xs) = (segs xs) ++ segs_aux xs [x] []

-- Funcion que calcula los segmentos consecutivos de una cadena
-- ejemplo:  segs_aux "lista" [] [] = ["","l","li","lis","list","lista"]

segs_aux [] acc segs = segs ++ [acc]
segs_aux (x:xs) acc segs = segs_aux xs (acc ++ [x]) (segs ++ [acc])

-- (b) --

-- Funcion solicitada, hace uso de elig, que es la funcion recursiva que
-- implementa lo que se pide.
-- Si la longitud de la cadena c es n, y el argumento es k, y k < n
-- entonces el numero de elementos que devuelve elige k c es: k + 1
-- si k >= n por definicion devolvemos la cadena c, tal cual en una lista

elige n xs =
		if (n == 0 || xs == []) then []
			else if (n >= longitud xs) then [xs] 
				else elig n 0 ((longitud xs) - n) (xs) []


-- Funcion que obtiene las subsecuencia de longitud n
-- Paramos en (-1) para poder meter el ultimo

elig (-1) _ _ _ l = l
elig n k r (x:xs) acc = elig (n-1) (k+1) (r+1) (x:xs) (acc ++ [ (take k (x:xs)) ++ (drop r (x:xs))] )


-- Calcula la longitud de una lista

longitud xs = long xs 0

-- Calcula la longitud con una acumulador

long [] n = n
long (x:xs) n = long xs (n+1)


----- Ejercicio 8 -----

type Escalar = Float
data Vector = Vec [Escalar] deriving Show
type Fila = [Escalar]
data Matriz = Mat [Fila] deriving Show


--- (a)

esMat::Matriz -> Bool

esMat (Mat []) = True
esMat (Mat (fila:filas)) = verificaFilas (longitud fila) filas


--Verifica que la lonfitud de las filas sea igual a n

verificaFilas _ [] = True
verificaFilas n (f:fs) = if (longitud f) /= n 
							then False 
								else verificaFilas n fs


--- (b)
negarMat:: Matriz -> Matriz
negarMat (Mat filas) = if (esMat (Mat filas)) 
							then (Mat (niegaFilas filas))
								else error "No es una matriz"

-- multiplica por -1 cada numero en una lista de listas de numeros

niegaFilas [] = []
niegaFilas (f:fs) = (map (\x -> -x) f) : (niegaFilas fs)

porMat :: Escalar -> Matriz -> Matriz
porMat a (Mat filas) = if (esMat (Mat filas)) 
						then Mat ((map (\fila -> map (\x-> a*x) fila)) filas)
							else error "El argumento no es una matriz"

--- (c)

mapMat :: (Escalar -> Escalar) -> Matriz -> Matriz
mapMat f (Mat filas) = if (esMat (Mat filas))
						then Mat ((map (\fila -> map (\x-> f x) fila)) filas)
							else error "EL argumento no es una matriz"

negarMat2 (Mat filas) = if (esMat (Mat filas)) 
							then mapMat (\x -> -x) (Mat filas)
								else error "El argumento no es una matriz"
porMat2 a (Mat filas) = if (esMat (Mat filas))
							then mapMat (\x -> a*x) (Mat filas)
								else error "El argumento no es una matriz"

--- (d)


porMixto::Matriz -> Vector -> Matriz
porMixto (Mat filas) (Vec vector) = if (not (esMat (Mat filas)))
								then error "El primer argumento no es una matriz"
									else if (numColumnas (Mat filas)) /= (longitud vector)
											then error "La longitud del vector y el numero de filas de la matriz no son iguales"
												else multiplicaPorVector (Mat filas) (Vec vector)

--Obtiene el numero de columnas de una matriz

numColumnas (Mat []) = 0
numColumnas (Mat (x:xs)) = longitud (x:xs)


--Hace la multiplicacion de una matriz por un vector, lo cual
-- nos devuelve una matriz

multiplicaPorVector (Mat filas) (Vec v) = Mat (g (map (\fila -> producto_aux fila v) filas))

-- Dada una lista de listas de numeros, hace un mapeo de cada
-- lista a la suma de los elementos de esa lista
-- ejemplo: g [[2,4],[5,6]] = [[6],[11]]
g (xs:xss) = map (\x -> [hazSuma x]) (xs:xss)


-- Obtiene una lista que es el producto de los elementos en orden
-- de dos listas de numeros. Las listas deben ser del mismo tamaño.
-- El uso de esta funcion denttro de multiplicaPorVector es seguro,
-- pues se checan los casos de error antes, en porMixto.
-- ejempl: producto_aux [1,3,5] [4,5,6] = [4,15,30]

producto_aux [] [] = []
producto_aux (f:fs) (v:vs) =  (f*v : producto_aux fs vs)

-- Obtiene la suma de una lista de numeros

hazSuma [] = 0
hazSuma (x:xs) = x + hazSuma xs



sumMat::Matriz -> Matriz -> Matriz

sumMat (Mat xs) (Mat ys) = if not (esMat (Mat xs)) then error "El primer argumento no es una matriz"
							else  if not (esMat (Mat ys)) then error "El segundo argumento no es una matriz"
								else if (numColumnas (Mat xs) == numColumnas (Mat ys)) && (numFilas (Mat xs) == numFilas (Mat ys))
										then Mat (sumaMatrices xs ys)
											else error "Las matrices no tienen las mismas dimensiones, no se pueden sumar."

-- Calcula la suma componente a componente de dos listas de listas de numeros
-- ejemplo: sumaMatrices [[1,2],[3,4]] [[4,7],[7,9]] = [[4,7],[11,16]]
-- Suponemos que las dos listas tienen la misma longitud
-- y que las listas internas tambien tienen la misma longitud

sumaMatrices [] [] = []
sumaMatrices (x:xs) (y:ys) = (sumaListasIguales x y) : sumaMatrices xs ys

-- Suma los elementos de dos listas, en orden, y los devuelve en otra lista
-- ejemplo sumaListasIguales [1,2,4] [2,5,7] = [3,7,11]

sumaListasIguales [] [] = []
sumaListasIguales (x:xs) (y:ys) = (x+y: sumaListasIguales xs ys)

-- Calcula el numero de filas de una matriz

numFilas (Mat xs) = longitud xs



--- (e)

foldMat::(Fila -> a -> a) -> a -> Matriz -> a

foldMat _ acc (Mat []) = acc
foldMat f acc (Mat (x:xs)) = foldMat f (f x acc) (Mat xs)


mayorMod::Matriz -> Fila

-- Se hace el test para saber si es una matriz, sólo por correctud,
-- pero no es necesario para obtener la fila cuyo modulo sea mayor

mayorMod (Mat xs) = if(esMat (Mat xs)) 
						then snd (foldMat (\ fila acc -> mantenMaxMod fila acc ) (0,[]) (Mat xs)) 
							else error "El argumento no es una matriz"


-- Funcion que devuelve la pareja cuyo modulo sea mayor

mantenMaxMod (x:xs) (modulo,ys) = if (calcMod (x:xs)) > modulo 
										then (calcMod (x:xs), (x:xs))
											else (modulo,ys)

-- Calcula el modulo de una lista de flotantes

calcMod (x:xs) = (calcMod_aux (x:xs))**(1/2)

-- Calcula la suma de los cuadrados de los elementos de una lista

calcMod_aux [] = 0
calcMod_aux (x:xs) = (x*x) + (calcMod_aux xs)






