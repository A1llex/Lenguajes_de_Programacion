--Lenguajes de Programacion
--Práctica 01
--Fernandez Aguilar Alex Gerardo

module Practica01 where 

--Definiciones
data Nat = Cero | Suc Nat deriving Show
--data ListaNat = Nil | Cons Nat ListaNat --deriving Show
--data BTree a = Void | Node ( BTree a ) a ( BTree a ) deriving Show
--data ListaSnoc a = Empty | Snoc ( ListaSnoc a ) a deriving Show

--Ejercicio 1
--
suma :: Nat  -> Nat  -> Nat
suma a Cero = a
suma Cero b = b
suma a (Suc b) = Suc(suma a b)