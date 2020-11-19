--Lenguajes de Programacion
--Práctica 02
--Fernandez Aguilar Alex Gerardo
--Pimentel Noriega Angel Christian
module Pract2 where
import EAB
type Substitution = ( Id , EAB )


--Ejercicio 2
-- La funcion regresara las variables libres de una EAB
freeVars :: EAB -> [Id]
freeVars (V x) = [x]
freeVars (N _) = []
freeVars (B _) = []
freeVars (Succ x) = freeVars x
freeVars (Pred x) = freeVars x
freeVars (Plus x y) = freeVars x ++  freeVars y
freeVars (Mul x y) = freeVars x ++  freeVars y
freeVars (IsZero x) = freeVars x
freeVars (If x y z) = freeVars x ++  freeVars y ++ freeVars z
freeVars (Let x a b ) = less (freeVars b) x
--Funcion Auxiliar
--para eliminar todas las incidencias de un id en una lista de los mismos
less :: [Id] -> Id -> [Id]
less [] _ = []
less (x:xs) y
 | x == y = less xs y
 |otherwise = x : (less xs y)

-- Sustitucion
substitution :: EAB -> Substitution -> EAB
substitution (V x) (id,value) 
 |  x == id = value
 | otherwise = (V x)
substitution (N x) _ = N x
substitution (B x) _ = B x
substitution (Succ x) s = Succ(substitution x s)
substitution (Pred x) s = Pred(substitution x s)
substitution (Plus x y) s = Plus (substitution x s) (substitution y s)
substitution (Mul x y) s = Mul (substitution x s) (substitution y s)
substitution (IsZero x) s = IsZero(substitution x s)
substitution (If con the els) s = If (substitution con s)(substitution the s)(substitution els s)
substitution (Let id1 value1 e) s@(id,value) 
 | id1 == id &&  notElem id (freeVars value) = Let id1 (substitution value1 s) (substitution e s)
 | otherwise = Let id1 value1 e


--Alpha equivalencia
--Compararemos 2 EAB si cambiando sus variables ligadas son la misma
alphaEq :: EAB -> EAB -> Bool
alphaEq (Let x1 y1 z1) (Let x2 y2 z2) = (substitution z1 (x1,y1) ) == (substitution z2 (x2,y2) )
alphaEq a b = (a == b)
