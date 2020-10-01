--Lenguajes de Programacion
--Práctica 01
--Fernandez Aguilar Alex Gerardo

module Practica01 where 

--Ejercicio 1
--Definiciones
data Nat = Cero | Suc Nat deriving (Eq,Show)

--Ejercicio 2
--
suma :: Nat  -> Nat  -> Nat
suma a Cero = a
suma Cero b = b
suma a (Suc b) = Suc(suma a b)

--Ejercicio 3
--Funcion que le restara al primer Natural el segundo Natural si el segundo es mayor igual regresa cero
sub :: Nat  -> Nat  -> Nat
sub Cero Cero = Cero
sub Cero b = Cero
sub (Suc a) (Suc b) = sub a b

--Ejercicio 4
--Funcion que Multiplica dos Naturales
mul :: Nat -> Nat -> Nat
mul Cero Cero = Cero
mul a Cero = Cero
mul Cero b = Cero
mul a (Suc Cero) = a
mul a (Suc b) = suma (a) (mul a b)

--Ejercicio 5
--funcion que revisa si cual es menor a cual n < m
menorQue :: Nat  -> Nat  -> Bool
menorQue Cero Cero = False
menorQue Cero b = True
menorQue (Suc a) (Suc b) = menorQue a b
menorQue a Cero = False

--Ejercicio 6
--funcion que revisa si son el mismo numero
eq :: Nat  -> Nat  ->Bool
eq Cero Cero = True
eq Cero b = False
eq a Cero = False
eq (Suc a) (Suc b) = eq a b

--Ejercicio 7
--
par :: Nat  ->Bool
par Cero = True
par (Suc Cero) = False
par (Suc (Suc a)) = par a 

--Ejercicio 8
--Revisa si es un Nat impar
impar :: Nat  ->Bool
impar = not . par 

--Ejercicio 9
--
toInt:: Nat -> Int
toInt Cero = 0
toInt (Suc a) = 1 + (toInt a)

--Ejercicio 10
--Convierte un entero a Natural
toNatError :: Int->  Nat
toNatError 0 = Cero
toNatError 1 = (Suc Cero)
toNatError n
 | (n < 0) = error "***  Exception: Hubo unerror."
 | otherwise = Suc( toNatError (pred n) )

--Ejercicio 11
--Convierte un Entero a un Natural con la seguridad de no detener la evalucion
toNatMaybe  ::Int-> Maybe Nat
toNatMaybe n
 | (n < 0) = Nothing    
 | otherwise = Just (toNatError n)


{- CONJUNTOS  -}

type Set a = [a] 
--deriving (Eq,Show)

--Ejericio 1
--Funcion que regresa si un elemento ya esta en el conjunto
pertenece  :: (Eq a)=>a -> Set a -> Bool
pertenece _ [] = False
pertenece a (x:xs) 
 | (a == x) = True
 | otherwise = pertenece a xs

--ejercicio 2
--Aseguramos que es un Conjunto
esConjunto  :: (Eq a)=> Set a ->Bool
esConjunto [] = True
esConjunto (x:xs) 
 | (pertenece x xs) = False
 | otherwise = esConjunto xs

--erjercicio 3
--Con una lista tendremos que quitar los elementos repetidos
toSet  :: (Eq a)=>[a] -> Set a
toSet [] = []
toSet (x:xs)
 | (pertenece x xs) = toSet xs
 | otherwise =  [x] ++ toSet xs

{-
--Ejercicio 4
--Determina si dos conjuntos son iguales
eq :: (Eq a,Ord a)=> Set a -> Set a ->Bool
eq [] [] = True
eq a b = eqaux (qsortc a) (qsortc b)

--Funcion auxiliar para solo ordenar una vez las listas de entrada
eqaux :: (Eq a)=>Set a -> Set a ->Bool
eqaux [] [] = True 
eqaux (x:xs) (y:ys) 
 | (x == y ) = eqaux xs ys
 | otherwise = False
eqaux _ _ = False

--Funcion Auxiliar de ordenamiento para reducir la complejidad de la busqueda de elementos repetidos 
qsortc :: (Ord a) => [a] -> [a]
qsortc [] = []
qsortc (x:xs) = qsortc [y|y <- xs, y <= x] ++ [x] ++ [y|y <- xs,y > x]
-}

--Ejercicio 5
--Todos
todos :: (Eq a ) => ( a -> Bool ) -> Set a -> Bool
todos f [] = True
todos f (x:xs) 
 | f x = todos f xs
 | otherwise = False

--Ejercicio 6
--
alguno :: (Eq a)=>Set a -> (a ->Bool) ->Bool
alguno [] f  = False
alguno (x:xs) f
 | f x = True
 | otherwise = alguno  xs f
