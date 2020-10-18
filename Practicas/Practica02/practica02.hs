--Lenguajes de Programacion
--Práctica 02
--Fernandez Aguilar Alex Gerardo
--Pimentel Noriega Angel Christian
--AlexGerardoFernandezAguilarAngelChristianPimentelNoriega 

type Id = String
type Substitution = ( Id , EAB )
data EAB = V Id | N Int | B Bool
            | Succ EAB | Pred EAB
            | Add EAB EAB | Mul EAB EAB
            | Not EAB | And EAB EAB | Or EAB EAB
            | Lt EAB EAB | Gt EAB EAB | Eq EAB EAB
            | If EAB EAB EAB
            | Let Id EAB EAB
            deriving (Eq)

--Ejercicio 1
--Define una instancia de la clase Show para el tipo de dato EAB
instance Show EAB where
    show (V x) = show x
    show (N x) = show x
    show (B x) = show x
    show (Succ x) = "(" ++ show x ++ "+1" ++")"
    show (Pred x) = "("++ show x ++ "-1" ++ ")"
    show (Add x y) = "("++show x ++ " + " ++ show y ++")"
    show (Mul x y) = "("++show x ++ " * " ++ show y ++ ")"
    show (Not x) = "("++"!"++ show x++")"
    show (And x y) = "("++show x ++ " && " ++ show y ++")"
    show (Or x y) = "("++show x ++ " || " ++ show y ++")"
    show (Lt x y) = "("++show x ++ " < " ++ show y ++")"
    show (Gt x y) = "("++show x ++ " > " ++ show y ++")"
    show (Eq x y) = "("++show x ++ " = " ++ show y ++")"
    show (If con the els) = "if("++ show con ++"){"++ show the ++"}else{"++ show els ++ "}"
    show (Let x value e) = show x ++" = "++ show value ++" in "++ show e 

--Ejercicio 2
-- La funcion regresara las variables libres de una EAB
freeVars :: EAB -> [Id]
freeVars (V x) = [x]
freeVars (N _) = []
freeVars (B _) = []
freeVars (Succ x) = freeVars x
freeVars (Pred x) = freeVars x
freeVars (Add x y) = freeVars x ++  freeVars y
freeVars (Mul x y) = freeVars x ++  freeVars y
freeVars (Not x) = freeVars x
freeVars (And x y) = freeVars x ++  freeVars y
freeVars (Or x y) = freeVars x ++  freeVars y
freeVars (Lt x y) = freeVars x ++  freeVars y
freeVars (Gt x y) = freeVars x ++  freeVars y
freeVars (Eq x y) = freeVars x ++  freeVars y
freeVars (If x y z) = freeVars x ++  freeVars y ++ freeVars z
freeVars (Let x a b ) = less (freeVars b) x
--Funcion Auxiliar
--para eliminar todas las incidencias de un id en una lista de los mismos
less :: [Id] -> Id -> [Id]
less [] _ = []
less (x:xs) y
 | x == y = less xs y
 |otherwise = x : (less xs y)

--Ejercicio 3
-- Sustitucion
substitution :: EAB -> Substitution -> EAB
substitution (V x) (id,value) |  x == id = value
                              | otherwise = (V x)
substitution (N x) (id,value) = N x
substitution (B x) (id,value) = B x
substitution (Succ x) (id,value) = Succ(substitution x (id,value))
substitution (Pred x) (id,value) = Pred(substitution x (id,value))
substitution (Add x y) (id,value) = Add (substitution x (id,value)) (substitution y (id,value))
substitution (Mul x y) (id,value) = Mul (substitution x (id,value)) (substitution y (id,value))
substitution (Not x) (id,value) = Not(substitution x (id,value))
substitution (And x y) (id,value) = And (substitution x (id,value)) (substitution y (id,value))
substitution (Or x y) (id,value) = Or (substitution x (id,value)) (substitution y (id,value))
substitution (Lt x y) (id,value) = Lt (substitution x (id,value)) (substitution y (id,value))
substitution (Gt x y) (id,value) = Gt (substitution x (id,value)) (substitution y (id,value))
substitution (Eq x y) (id,value) = Eq (substitution x (id,value)) (substitution y (id,value))
substitution (If con the els) (id,value) = If (substitution con (id,value))(substitution the (id,value))(substitution els (id,value))
substitution (Let id1 value1 e) (id,value) | id1 == id = (Let id1 value1 e)
                                           | otherwise = (Let id1 (substitution value1 (id,value)) (substitution e (id,value))) 

--Ejercicio 4
--Compararemos 2 EAB si cambiando sus variables ligadas son la misma
alphaEq :: EAB -> EAB -> Bool
alphaEq a@(Let x1 y1 z1) b@(Let x2 y2 z2) = (substitution z1 (x1,y1) ) == (substitution z2 (x2,y2) )
alphaEq a b = (a == b)

--Optimizacion 1
--Funcion que resolvera todas las operacion aritmeticas explicitas
aFolding :: EAB -> EAB
aFolding (N x) = N x
aFolding (V x) = V x
aFolding (B x) = B x
aFolding (Succ (N x)) = N(x+1) 
aFolding (Succ x) = Succ(aFolding x)
aFolding (Pred (N x)) = N(x-1) 
aFolding (Pred x) = Pred(aFolding x)
aFolding (Add (N x) (N y)) = N(x+y)
aFolding (Add x y)
 | x == (N 0) = aFolding y
 | y == (N 0) = aFolding x
 |otherwise = Add(aFolding x) (aFolding y)
aFolding (Mul (N x) (N y)) = (N (x*y))
aFolding (Mul x y)
 | x == (N 1) = aFolding y
 | y == (N 1) = aFolding x
 |otherwise = Mul(aFolding x) (aFolding y)
aFolding (Not x) = Not(aFolding x)
aFolding (And x y) = And (aFolding x) (aFolding y)
aFolding (Or x y) = Or (aFolding x) (aFolding y)
aFolding (Lt x y) = Lt(aFolding x) (aFolding y)
aFolding (Gt x y) = Gt(aFolding x) (aFolding y)
aFolding (Eq x y) = Eq(aFolding x) (aFolding y)
aFolding (If con the els) = If(aFolding con) (aFolding the) (aFolding els)
aFolding (Let x y z) = aFolding(substitution z (x,y))

--Optimizacion 2
--Funcion que resolvera todas las operacion Boleanas explicitas
bFolding :: EAB -> EAB 
bFolding a = bFolding2(bFolding2(a))

bFolding2 :: EAB -> EAB 
bFolding2 (N x) = N x
bFolding2 (V x) = V x
bFolding2 (B x) = B x
bFolding2 (Succ x) = Succ (bFolding2 x) 
bFolding2 (Pred x) = Pred (bFolding2 x) 
bFolding2 (Add x y) = Add (bFolding2 x) (bFolding2 y)
bFolding2 (Mul x y) = Mul (bFolding2 x) (bFolding2 y)
bFolding2 (Not (B x)) = B (not x)
bFolding2 (Not x) = Not(bFolding2 x)
bFolding2 (And (B x) (B y)) = B (x && y)
bFolding2 (And x y)
 | x == (B False) = B False
 | y == (B False) = B False
 | x == (B True) = bFolding2 y
 | y == (B True) = bFolding2 x
 |otherwise = And ( bFolding2 x) ( bFolding2 y)
bFolding2 (Or (B x) (B y)) = B (x && y)
bFolding2 (Or x y)
 | x == (B True) = B True
 | y == (B True) = B True
 | x == (B False) = bFolding2 y
 | y == (B False) = bFolding2 x
 |otherwise = Or (bFolding2 x) (bFolding2 y)
bFolding2 (Lt (N x) (N y)) = B (x < y)
bFolding2 (Lt x y) = Lt(bFolding2 x) (bFolding2 y)
bFolding2 (Gt (N x)(N y)) = B (x > y)
bFolding2 (Gt x y) = Gt(bFolding2 x) (bFolding2 y)
bFolding2 (Eq (N x)(N y)) = B (x == y)
bFolding2 (Eq x y) = Eq(bFolding2 x) (bFolding2 y)
bFolding2 (If (B x) y z) 
 | x = bFolding2 y
 | otherwise = (bFolding2 z)
bFolding2 (If x y z) = If(bFolding2 x) (bFolding2 y) (bFolding2 z)
bFolding2 (Let x y z) = bFolding2(substitution z (x,y))
