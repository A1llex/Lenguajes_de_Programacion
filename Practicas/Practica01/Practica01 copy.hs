--Lenguajes de Programacion
--Práctica 01
--Fernandez Aguilar Alex Gerardo

module Practica01 where 

--Definiciones
data Natural = Cero | Suc Natural deriving Show
data ListaNat = Nil | Cons Natural ListaNat --deriving Show
data BTree a = Void | Node ( BTree a ) a ( BTree a ) deriving Show
data ListaSnoc a = Empty | Snoc ( ListaSnoc a ) a deriving Show


--Ejercicio 1
--Funcion que regresa si un numero "Natural" es estrictamente mayor que otro
mayorQue :: Natural -> Natural -> Bool
mayorQue Cero Cero = False
mayorQue Cero b = False
mayorQue a Cero = True
mayorQue (Suc a) (Suc b)  = mayorQue a b


--Ejercicio 2
--Funcion que le restara al primer Natural el segundo Natural
restaNat :: Natural -> Natural -> Natural
restaNat Cero Cero = error "El Ejercicio Pide que sea estrictamente mayor el primer Natural"
restaNat Cero b = error "Es necesario que el primer Natural sea mayor al segundo Natural"
restaNat a Cero = a
restaNat (Suc a) (Suc b) = restaNat a b

--Ejercicio 3
--Funcion que Multiplica dos Naturales
mulNat :: Natural -> Natural -> Natural
mulNat Cero Cero = Cero
mulNat a Cero = Cero
mulNat Cero b = Cero
mulNat a (Suc Cero) = a
mulNat a (Suc b) = sumNat (a) (mulNat a b)

--Funcion auxiliar Suma de dos Naturales
sumNat :: Natural -> Natural -> Natural
sumNat a Cero = a
sumNat Cero b = b
sumNat a (Suc b) = Suc(sumNat a b)

--Ejercicio 4
--Funcion que regresa la reversa de una lista
reversa :: ListaNat -> ListaNat
reversa Nil = Nil
reversa (Cons a  Nil) = (Cons a  Nil)
reversa (Cons a  bs) = concatena (reversa bs)  (Cons a Nil) 

--Ejercicio 5
--Funcion que concatena dos LisaNat una seguida de la segunda
concatena :: ListaNat -> ListaNat -> ListaNat
concatena Nil b = b
concatena a Nil = a
concatena (Cons a as) b =  Cons ( a ) (concatena  as  b)

--Ejercicio 6
--Funcion que busca e indica si un Natural esta dentro de una listaNat
pertenece :: Natural -> ListaNat ->Bool
pertenece a  Nil = False
pertenece a  (Cons b bs) 
 | (igual a b) = True
 | otherwise = (pertenece a bs)

--Funcion auxiliar para saber si dos Naurales son el mismo 
igual :: Natural -> Natural -> Bool
igual Cero Cero = True
igual a Cero = False
igual Cero b = False
igual (Suc a) (Suc b) = igual a b

--Ejecicio 7
--Funcion que transformara un arbol en una lista
inOrden :: BTree a -> [a]
inOrden Void  = []
inOrden (Node (ai) e (ad)) = (inOrden ai) ++ [e] ++ (inOrden ad)

--Ejercicio 8
--Funcion que agrega un elemento a un arbol ordenado
agregaOrden :: (Ord a ) => a -> BTree a -> BTree a
agregaOrden a ( Node (Void) (e) (Void) )
 | (a<=e) = ( Node (Node(Void)(a)(Void)) (e) (Void) )
 | otherwise = ( Node (Void) (e) (Node(Void)(a)(Void)) )
agregaOrden a ( Node(ai)(e)(ad))
 | (a<=e) = ( Node ( agregaOrden (a) (ai) ) (e) (ad) )
 | otherwise = ( Node (ai) (e) ( agregaOrden (a) (ad) ) )

--Ejercicio 9 
--Funcion que regresa una listaSnoc la devuelve sin la cabeza
tailSnoc :: ListaSnoc a -> ListaSnoc a
tailSnoc (Snoc (Empty) a) = Empty
tailSnoc (Snoc (e)  a) = (Snoc (tailSnoc e) a)

--Ejercicio 10
--Funcion que implementa mapeo sobre listas snoc
mapSnoc :: ( a -> b ) -> ListaSnoc a -> ListaSnoc b
mapSnoc (f) (Snoc (Empty) e) = (Snoc (Empty) (f e))
mapSnoc (f) (Snoc (snl) e) = (Snoc (mapSnoc (f) (snl) ) (f e))

--Puntos Extra
-- 1 
--Funcion que regresa el largo de una cadena de Int
--La funcion solo funciona con naturales sin incluir al cero
longitud :: Int -> Int
longitud 0 = 0
longitud (a) = 1 + (longitud (div a  10))

-- 2
--Funcion como fibonacci pero suma los 3 anteriores
tribonaccies :: Int -> [Int] 
tribonaccies 0 = [0]
tribonaccies 1 = [0,1]
tribonaccies 2 = [0,1,1]
tribonaccies n = (tribonaccies (n-1)) ++ [sumtreslist (tribonaccies ( n-1) )] 

--Funcion auxiliar que suma los ultimos 3 elementos de las listas
sumtreslist :: [Int] -> Int
sumtreslist [] = 0
sumtreslist [a] = a
sumtreslist [a,b] = a+b
sumtreslist [a,b,c] = a+b+c
sumtreslist (a:b) = sumtreslist( b)