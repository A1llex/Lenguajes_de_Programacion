--Lenguajes de Programacion
--Práctica 01
--Fernandez Aguilar Alex Gerardo

module Practica01 where 

--Ejercicio 1
--Definiciones
data Nat = Cero | Suc Nat deriving (Eq,Show)
--data ListaNat = Nil | Cons Nat ListaNat --deriving Show
--data BTree a = Void | Node ( BTree a ) a ( BTree a ) deriving Show
--data ListaSnoc a = Empty | Snoc ( ListaSnoc a ) a deriving Show

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
par (Suc a) (Suc b) = par a b

--Ejercicio 8
--
impar :: Nat  ->Bool
impar = not par

--Ejercicio 9
--
toInt:: Nat -> Int
toInt Cero = 0
toInt (Suc a) = 1 + (to int a)

--Ejercicio 10