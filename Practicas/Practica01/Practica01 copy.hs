--Lenguajes de Programacion
--Práctica 01
--Fernandez Aguilar Alex Gerardo
--Pimentel Noriega Angel Christian

--{Ejercicio 1}
--{Definiendo los numeros naturales}
data Nat = Cero
    | Suc Nat
    deriving(Eq, Show)

--Ejercicio 2
--suma dos naturales
suma :: Nat  -> Nat  -> Nat
suma a Cero = a
suma Cero b = b
suma a (Suc b) = Suc(suma a b)


--{Ejercicio 3}
--Resta de dos naturales
sub :: Nat -> Nat -> Nat
sub x Cero = x
sub (Suc x) (Suc y) = sub x y

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
menorQue :: Nat -> Nat -> Bool
menorQue Cero Cero = False
menorQue Cero y = True
menorQue x Cero = False
menorQue (Suc x) (Suc y) = menorQue x y

--Ejercicio 6
--funcion que revisa si son el mismo numero
eq :: Nat  -> Nat  ->Bool
eq Cero Cero = True
eq Cero b = False
eq a Cero = False
eq (Suc a) (Suc b) = eq a b


--{Ejercicio 7}
--Verifica si un Natural es par
par :: Nat -> Bool
par Cero = True
par (Suc Cero) = False
par (Suc (Suc x)) = par x

--Ejercicio 8
--Revisa si es un Nat impar
impar :: Nat  ->Bool
impar = not . par 

--{Ejercicio 9}
--Convierte un natural a un entero
toInt :: Nat -> Int
toInt Cero = 0
toInt (Suc x) = 1 + toInt(x)

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