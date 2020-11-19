--Transpiladores

type State = Id -> Value

type Id  = String

data Value = Nat Int | Boolean Bool

data EB1 = Tt1 | Ff1
            | VarB1 Id
            | Not EB1 | And EB1 EB1
            | Let Id EB1 EB1
            deriving (Show,Eq)

data EB2 = Tt2 | Ff2
            | VarB2 Id
            | If EB2 EB2 EB2
            | Where EB2 Id EB2
            deriving (Show,Eq)

toEB2 :: EB1 -> EB2
toEB2 Tt1 = Tt2
toEB2 Ff1 = Ff2
toEB2 (VarB1 x) = VarB2 x
-- Evaluamos a e y si es True entonces por el not deberia de salir false, en otro caso True
toEB2 (Not e) = If (toEB2 e) Ff2 Tt2
-- Usamos dos if anidados si las dos condiciones del if son correctas entonces es como si hubiera un and
-- si en alguno de los dos es falso entonces regresamos falso
toEB2 (And e1 e2) = If (toEB2 e1) (If(toEB2 e2) Tt2 Ff2) Ff2
-- Reacomodamos los parametros solamente
toEB2 (Let x e1 e2) = Where (toEB2 e1) x (toEB2 e2)


toEB1 :: EB2 -> EB1
toEB1 Tt2 = Tt1
toEB1 Ff2 = Ff1
toEB1 (VarB2 x) = VarB1 x
-- Iba a usar la equivalencia de p -> q = ¬p v q pero no sabia que hacer con
-- el else y ya no me dió tiempo de hacer algo más
-- toEB1 (If condition e1 e2) =
toEB1 (Where e1 x e2) = Let x (toEB1 e1) (toEB1 e2)

eval1 :: EB1 -> EB1
eval1 Tt1 = Tt1
eval1 Ff1 = Ff1
eval1 (VarB1 x) = VarB1 x
eval1 (Not e) = case (eval1 e) of
                    Tt1 -> Ff1
                    Ff1 -> Tt1
                    _ -> Not (eval1 e)
eval1 (And e1 e2) = case (eval1 e1) of
                        Tt1 -> case (eval1 e2) of
                                Tt1 -> Tt1
                                Ff1 -> Ff1
                                _ -> And Tt1 (eval1 e2)
                        Ff1 -> Ff1
                        _ -> And (eval1 e1) (eval1 e2)
eval1 (Let x e1 e2) = eval1 (substitution1 e2 (x, e1))
--La evalucación del Let no me salió ya que no supe como 
-- cambiar de un Value a un EB1 en la funcion auxiliar update
eval2 :: State -> EB1 -> EB1
eval2 _ Tt1 = Tt1
eval2 _ Ff1 = Ff1
eval2 s (VarB1 x) = case(s x) of
                        Boolean True -> Tt1
                        Boolean False -> Ff1
                        _ -> error"Integer not implemented in EB1"
eval2 s (Not e) = case (eval2 s e ) of
                      Tt1 -> Ff1
                      Ff1 -> Tt1
                      _ -> Not (eval2 s e)
eval2 s (And e1 e2) = case (eval2 s e1) of
                        Tt1 -> case (eval2 s e2) of
                                 Tt1 -> Tt1
                                 Ff1 -> Tt1
                                 _ -> And Tt1 (eval2 s e2)
                        Ff1 -> Ff1
                        _ -> And (eval2 s e1) (eval2 s e2)
-- eval2 s (Let x e1 e2) = eval2 (update s x (eval2 s e1)) e2

eval3 :: EB2 -> EB2
eval3 Tt2 = Tt2
eval3 Ff2 = Ff2
eval3 (VarB2 x) = VarB2 x
eval3 (If condition e1 e2) = case (eval3 condition) of 
                                Tt2 -> eval3 e1
                                Ff2 -> eval3 e2
                                _ -> If (eval3 condition) (eval3 e1) (eval3 e2)
eval3 (Where e1 x e2) = eval3 (substitution2 e2 (x, e1))
--La evalucación del Where no me salió ya que no supe como 
-- cambiar de un Value a un EB2 en la funcion auxiliar update
eval4 :: State -> EB2 -> EB2
eval4 _ Tt2 = Tt2
eval4 _ Ff2 = Ff2
eval4 s (VarB2 x) = case(s x) of
                        Boolean True -> Tt2
                        Boolean False -> Ff2
                        _ -> error"Integer not implemented in EB2"
eval4 s (If condition e1 e2) = case (eval4 s condition) of
                                  Tt2 -> eval4 s e1
                                  Ff2 -> eval4 s e2
                                  _ -> If (eval4 s condition) (eval4 s e1) (eval4 s e2)
-- eval4 (Where e1 x e2) = eval2 (update s x (eval4 s e1)) e2

-- Uso una fucion auxiliar pero depende de el where y el if no implementado 
evalEB1 :: EB1 -> Bool
evalEB1 x = evalAUX1(toEB2 x)

evalAUX1 :: EB2 -> Bool
evalAUX1 Tt2 = True
evalAUX1 Ff2 = False
evalAUX1 (VarB2 _) = error"Variable Global no soportada"
evalAUX1 (If condition e1 e2) = evalAUX1(eval3(If condition e1 e2))
evalAUX1 (Where e1 x e2) = evalAUX1(eval3(Where e1 x e2))

-- Uso una funcion auxiliar pero no esta implementado el Let correctamente
evalEB2 :: EB2 -> Bool
evalEB2 x = evalAUX2(toEB1 x)

evalAUX2 :: EB1 -> Bool
evalAUX2 Tt1 = True
evalAUX2 Ff1 = False
evalAUX2 (VarB1 _)= error"Variable Global no soportada"
evalAUX2 (Not e) = not(evalAUX2 e)
evalAUX2 (And e1 e2) = (evalAUX2 e1) && (evalAUX2 e2)
evalAUX2 (Let x e1 e2) = evalAUX2(eval1(Let x e1 e2))

-- Funcion auxiliar para actualizar estados
update :: State -> Id -> Value -> State
update s x n y = if x == y then n else s y

-- Sustitucion EB1
type Substitution1 = ( Id , EB1 )

substitution1 :: EB1 -> Substitution1 -> EB1
substitution1 (VarB1 x) (id,value) |  x == id = value
                              | otherwise = (VarB1 x)
substitution1 Tt1 _ = Tt1
substitution1 Ff1 _ = Ff1
substitution1 (Not x) (id,value) = Not(substitution1 x (id,value))
substitution1 (And x y) (id,value) = And(substitution1 x (id,value)) (substitution1 y (id,value))
substitution1 (Let id1 value1 e) (id,value) | id1 == id &&  notElem id (freeVars1 value) = Let id1 (substitution1 value1 (id,value)) (substitution1 e (id,value))
                                           | otherwise = Let id1 value1 e
-- La funcion regresara las variables libres de una EB1
freeVars1 :: EB1 -> [Id]
freeVars1 (VarB1 x) = [x]
freeVars1 Tt1 = []
freeVars1 Ff1 = []
freeVars1 (Not x) = freeVars1 x
freeVars1 (And x y) = freeVars1 x ++  freeVars1 y
freeVars1 (Let x a b ) = less (freeVars1 b) x

-- Sustitucion EB2
type Substitution2 = ( Id , EB2 )

substitution2 :: EB2 -> Substitution2 -> EB2
substitution2 (VarB2 x) (id,value) |  x == id = value
                              | otherwise = (VarB2 x)
substitution2 Tt2 _ = Tt2
substitution2 Ff2 _ = Ff2
substitution2 (If con the els) (id,value) = If (substitution2 con (id,value))(substitution2 the (id,value))(substitution2 els (id,value))
substitution2 (Where value1 id1 e) (id,value) | id1 == id &&  notElem id (freeVars2 value) = Where (substitution2 value1 (id,value)) id1 (substitution2 e (id,value))
                                           | otherwise = Where  value1 id1 e

-- La funcion regresara las variables libres de una EB2
freeVars2 :: EB2 -> [Id]
freeVars2 (VarB2 x) = [x]
freeVars2 Tt2 = []
freeVars2 Ff2 = []
freeVars2 (If x y z) = freeVars2 x ++  freeVars2 y ++ freeVars2 z
freeVars2 (Where a x b ) = less (freeVars2 b) x

--Funcion Auxiliar
--para eliminar todas las incidencias de un id en una lista de los mismos
less :: [Id] -> Id -> [Id]
less [] _ = []
less (x:xs) y
 | x == y = less xs y
 |otherwise = x : (less xs y)