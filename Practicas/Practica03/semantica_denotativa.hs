--Semantica Denotativa
-- No lo importe desde EAB porque no encontraba el import
import EAB
import Pract2

type State = Id -> Value
data Value = Nat Int | Boolean Bool


instance Show Value where
    show (Nat x) = show x
    show (Boolean x) = show x



dEval1 :: EAB -> Value
dEval1 (V _) = error "La expresión contiene variables globales"
dEval1 (N n) = Nat n
dEval1 (B b) = Boolean b
dEval1 (Succ e) = case (dEval1 e) of
                    (Nat x) ->  Nat (x+1)
                    _ -> error"Sucesor de Booleano no implementado"
dEval1 (Pred e) = dEval1 e
dEval1 (Plus e1 e2) = case(dEval1 e1) of
                        Nat x -> case (dEval1 e2) of
                                    Nat y -> Nat (x+y)
                                    _ -> error"Suma de Booleanos no implementada"
                        _ -> error"Suma de Booleanos no implementada"
dEval1 (Mul e1 e2) = case(dEval1 e1) of
                        Nat x -> case (dEval1 e2) of
                                    Nat y -> Nat (x*y)
                                    _ -> error"Multiplicacion de Booleanos no implementada"
                        _ -> error"Multiplicacion de Booleanos no implementada"
dEval1 (IsZero e) = case (dEval1 e) of
                        (Nat 0) -> Boolean True
                        _ -> Boolean False
dEval1 (If condition e1 e2) = case (dEval1 condition) of
                                (Boolean True) -> dEval1 e1
                                _ -> dEval1 e2
dEval1 (Let x e1 e2) = dEval1 (substitution e2 (x, e1))

dEval2 :: State -> EAB -> Value
dEval2 s (V x) = s x
dEval2 _ (N n) = Nat n
dEval2 _  (B b) = Boolean b
dEval2 s (Succ e) = case(dEval2 s e) of
                        (Nat x) -> Nat (x+1)
                        _ -> error"Sucesor de Booleano no implementado"
dEval2 s (Pred e) = dEval2 s e
dEval2 s (Plus e1 e2) = case(dEval2 s e1) of
                        Nat x -> case (dEval2 s e2) of
                                    Nat y -> Nat (x+y)
                                    _ -> error"Suma de Booleanos no implementada"
                        _ -> error"Suma de Booleanos no implementada"
dEval2 s (Mul e1 e2) = case(dEval2 s e1) of
                        Nat x -> case (dEval2 s e2) of
                                    Nat y -> Nat (x*y)
                                    _ -> error"Multiplicacion de Booleanos no implementada"
                        _ -> error"Multiplicacion de Booleanos no implementada"
dEval2 s (IsZero e) = case (dEval2 s e) of
                        (Nat 0) -> Boolean True
                        _ -> Boolean False
dEval2 s (If condition e1 e2) = case (dEval2 s condition) of
                                (Boolean True) -> dEval2 s e1
                                _ -> dEval2 s e2
dEval2 s (Let x e1 e2) = dEval2 (update s x (dEval2 s e1)) e2

-- Funcion auxiliar para actualizar estados
update :: State -> Id -> Value -> State
update s x n y = if x == y then n else s y
