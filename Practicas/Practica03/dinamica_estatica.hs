--Lenguajes de Programacion
--Práctica 03
--Fernandez Aguilar Alex Gerardo
--Pimentel Noriega Angel Christian
import EAB
import Pract2

data Type = Nat | Boolean deriving (Eq)
type Ctx = [(Id, Type)]

--Semantica Dinamica
--Eval1
--Funcion que representa Semantica de paso pequeño
--evaluara solo un paso a la vez y valuando primero el lado izquierdo al derecho
eval1 :: EAB -> EAB
eval1 (V x) = V x
eval1 (N n) = N n
eval1 (B b) = B b
eval1 (Succ n) = case n of
    (N a) -> N (a+1)
    _ -> Succ (eval1 n)
eval1 (Pred n) = case n of
    (N a) -> N (a-1)
    _ -> Succ (eval1 n)
eval1 (Plus x y) = case (x,y) of 
    (N a ,N b) -> N (a+b) 
    (N a , _) -> Plus (N a) (eval1 y)
    _ -> Plus (eval1 x) y
eval1 (Mul x y) = case (x,y) of 
    (N a ,N b) -> N (a * b) 
    (N a , _) -> Mul (N a) (eval1 y)
    _ -> Mul (eval1 x) y
eval1 (IsZero n) = case n of
    (N 0) -> B True
    (N x) -> B False
    _ -> IsZero (eval1 n)
-- Valuamos la condicion hasta que nos de un booleano y si ya es true o false regresamos b o c sin valuar 
-- en otro caso seguimos valuando la condicion 
eval1 (If a b c)  = case a of
    (B True) -> b
    (B False) -> c
    _ -> If (eval1 a) b c
--Valuamos el let solo cuando 
eval1 (Let id a b) = case a of
    --Valuamos con la sustitucion de la practica 2
    (N x) -> substitution ( b) (id,a)
    --Seguimos valuando a
    _ -> Let id (eval1 a) b 
{-
    Aclaracion , el ejemplo dos de la practica dice 0 pero el sucesor de 0 es 1 
    y como solo estamos valuando un paso a la vez en este segundo ejemplo quedaria :
         Let " x " = 3 in (" x " + (0+1))
-}

--Evals
--Funcion que represantaria el Semantica de Paso grande 
evals :: EAB -> EAB
evals (V x) = V x
evals (N n) = N n
evals (B b) = B b
evals (Succ x) = eval1 (Succ (evals x) )
evals (Pred x) = eval1  (Pred  (evals x) )
evals (Plus x y) =  eval1 (Plus (evals x) (evals y) )
evals (Mul x y) = eval1 (Mul (evals x) (evals y) )
evals (IsZero x) = eval1 (IsZero (evals x) )
evals (If x y z) = eval1 (If (evals x) (evals y) (evals z) )
evals (Let id x y) = eval1 (eval1 (Let id (evals x) (evals y) ))

--is Valid
--Funcion que revisa si se regresa un valor 
isValid :: EAB -> Bool
isValid a = case  (evals a)  of
    (N x) -> True
    _ -> False

--Semantica Estatica
--Funcion
vt :: Ctx -> EAB -> Type -> Bool
vt [] eab tipo =  
    if (isValid eab) 
    then if (tipo == Nat) 
        then True
        else False
    else if (tipo == Nat) 
        then False
        else True
vt [(_, a)] eab tipo = (a == tipo)
vt ((_, a):xs) eab tipo = (a == tipo)