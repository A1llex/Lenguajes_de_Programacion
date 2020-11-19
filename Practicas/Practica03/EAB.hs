--Lenguajes de Programacion
--Práctica 03
--Fernandez Aguilar Alex Gerardo
--Pimentel Noriega Angel Christian
module EAB where

type Id  = String

data EAB = V Id | N Int | B Bool
           | Succ EAB | Pred EAB
           | Plus EAB EAB | Mul EAB EAB | IsZero EAB
           | If EAB EAB EAB | Let Id EAB EAB 
           deriving (Eq)


instance Show EAB where
    show (V x) = show x
    show (N x) = show x
    show (B x) = show x
    show (Succ x) = "(" ++ show x ++ "+1" ++")"
    show (Pred x) = "("++ show x ++ "-1" ++ ")"
    show (Plus x y) = "("++show x ++ " + " ++ show y ++")"
    show (Mul x y) = "("++show x ++ " * " ++ show y ++ ")"
    show (IsZero x) = "("++"cero?"++ show x++")"
    show (If con the els) = "if("++ show con ++"){"++ show the ++"}else{"++ show els ++ "}"
    show (Let x value e) =  "Let "++ show x ++" = "++ show value ++" in "++ show e 
