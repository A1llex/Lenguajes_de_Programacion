--Lenguajes de Programacion
--Práctica 01
--Fernandez Aguilar Alex Gerardo
--Pimentel Noriega Angel Christian

 --{ CONJUNTOS  }-
type Set a  = [a] 

--Ejercicio 1
-- Pertenece
pertenece :: (Eq a) => a -> Set a -> Bool
pertenece a [] = False
pertenece a (x:xs) 
 | a == x = True
 | otherwise = pertenece a xs

 --ejercicio 2
--Aseguramos que es un Conjunto
esConjunto  :: (Eq a)=> Set a ->Bool
esConjunto [] = True
esConjunto (x:xs) 
 | (pertenece x xs) = False
 | otherwise = esConjunto xs

--Ejercicio 3
-- toSet
toSet :: (Eq a ) => [ a ] -> Set a
toSet [] = []
toSet (x:xs) 
 | pertenece x xs = toSet xs
 | otherwise = x :( toSet xs)

--Ejercicio 4
--Determina si dos conjuntos son iguales

eq :: (Eq a,Ord a)=> Set a -> Set a ->Bool
eq [] [] = True
eq a b = (qsortc a)  ==  (qsortc b)

--Funcion Auxiliar de ordenamiento para reducir la complejidad de la busqueda de elementos repetidos 
qsortc :: (Ord a) => [a] -> [a]
qsortc [] = []
qsortc (x:xs) = (qsortc [ y | y < - xs, y <= x]) ++ [x] ++  (qsortc[y | y <- xs,y > x])

--Ejercicio 5
--Todos
todos :: (Eq a ) => ( a -> Bool ) -> Set a -> Bool
todos f [] = True
todos f (x:xs) 
 | f x = todos f xs
 | otherwise = False

--Ejercicio 6
--revisa si al menos uno cumple 
alguno :: (Eq a)=>Set a -> (a ->Bool) ->Bool
alguno [] f  = False
alguno (x:xs) f
 | f x = True
 | otherwise = alguno  xs f

--Ejercicio 7
-- Agrega
agrega :: (Eq a ) => a -> Set a -> Set a
agrega a [] = [a]
agrega a x 
 | pertenece a x = x
 | otherwise = a : x

--Ejercicio 8
--realiza la union de dos conjuntos
union:: (Eq a)=>Set a -> Set a -> Set a
union a [] = a
union [] a = a
union x y = toSet (x++y) 

--Ejercicio 9
--realiza la interseccion de dos conjuntos
interseccion :: (Eq a)=>Set a -> Set a -> Set a
interseccion [] _ = []
interseccion _ [] = []
interseccion (x:xs) y
 | (pertenece x y) = x : (interseccion xs y)
 | otherwise = interseccion xs y

 --Ejercicio 10
--realiza la diferencia A-B
diferencia  :: (Eq a)=>Set a -> Set a -> Set a
diferencia [] _ = []
diferencia a [] =  a
diferencia (x:xs) y
 | (pertenece x y) = (diferencia xs y)
 | otherwise = x :(diferencia xs y)

--Ejercicio 11
-- esSubconjunto
esSubconjunto :: (Eq a ) => Set a -> Set a -> Bool
esSubconjunto [] _  = True
esSubconjunto _ [] = False
esSubconjunto (x:xs) (y:ys) 
 | pertenece x (y:ys) = esSubconjunto xs (y:ys)
 | otherwise = False