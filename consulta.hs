module Consulta where

import Predicado
import ExprBuilder
import Utils

type Elemento = String
type Tupla = [Elemento]
type Atributo = String
data RelacionBase = Relacion { atributos :: [(Atributo, Atributo)], tuplas :: [Tupla] }

data Consulta
    = Taux String
    | Tau RelacionBase
    | Sigma Predicado Consulta
    | Pi [Atributo] Consulta
    | Rho String Consulta
    | X Consulta Consulta
    | NX Consulta Consulta
    | U Consulta Consulta
    | I Consulta Consulta
    | D Consulta Consulta

instance Show Consulta where
    show (Taux tb)   = tb
    show (Tau tb)    = (listToCsvLine $ (snd) <$> atributos tb) ++ "\n" ++ (unlines $ listToCsvLine <$> tuplas tb)
    show (Sigma p q) = "σ [" ++ (show p) ++ "] " ++ (show q)
    show (Pi cs q)   = "π [" ++ (init $ foldr ((++).(++",")) "" cs) ++ "] " ++ (show q)
    show (Rho t q)   = "⍴ [" ++ t ++ "] " ++ (show q)
    show (X q1 q2)   = "(" ++ (show q1) ++ " ✕ " ++ (show q2) ++ ")"
    show (NX q1 q2)  = "(" ++ (show q1) ++ " ⋈ " ++ (show q2) ++ ")"
    show (U q1 q2)   = "(" ++ (show q1) ++ " ∪ " ++ (show q2) ++ ")"
    show (I q1 q2)   = "(" ++ (show q1) ++ " ∩ " ++ (show q2) ++ ")"
    show (D q1 q2)   = "(" ++ (show q1) ++ " − " ++ (show q2) ++ ")"

instance Read Consulta where
    readsPrec _ str =
        let
            readConsulta (qb,[]) = (qb,[])
            readConsulta (qb,('(':cs)) = readConsulta (addExpr qb $ getExpr $ fst $ readConsulta (createExprBuilder, fst $ parens ('(':cs)), trim $ snd $ parens ('(':cs))
            readConsulta (qb,('✕':cs)) = readConsulta (addBinaryOp qb X, trim cs)
            readConsulta (qb,('⋈':cs)) = readConsulta (addBinaryOp qb NX, trim cs)
            readConsulta (qb,('∪':cs)) = readConsulta (addBinaryOp qb U, trim cs)
            readConsulta (qb,('∩':cs)) = readConsulta (addBinaryOp qb I, trim cs)
            readConsulta (qb,('−':cs)) = readConsulta (addBinaryOp qb D, trim cs)
            readConsulta (qb,('σ':cs)) = readConsulta (addUnaryOp qb $ Sigma (read (fst $ firstKeysExp cs) :: Predicado), trim $ snd $ firstKeysExp cs)
            readConsulta (qb,('π':cs)) = readConsulta (addUnaryOp qb $ Pi (words $ (\c -> if c == ',' then ' ' else c) <$> (fst $ firstKeysExp cs)), trim $ snd $ firstKeysExp cs)
            readConsulta (qb,('⍴':cs)) = readConsulta (addUnaryOp qb $ Rho (fst $ firstKeysExp cs), trim $ snd $ firstKeysExp cs)
            readConsulta (qb,s) =
                let w = words s
                in if length w > 1
                    then readConsulta (addExpr qb $ Taux $ head w, trim $ drop (length $ head w) s)
                    else (addExpr qb $ Taux $ head w, "")
        in [(getExpr $ fst $ readConsulta (createExprBuilder, fixSpaces str), "")]



crearConsulta :: String -> Consulta
crearConsulta s = read s :: Consulta

crearRelacion :: [(String,String)] -> [[String]] -> RelacionBase
crearRelacion a t = Relacion a t



foldConsulta :: (String -> Consulta) -> (RelacionBase -> Consulta) -> (Predicado -> Consulta -> Consulta) -> ([Atributo] -> Consulta -> Consulta) -> (String -> Consulta -> Consulta) -> (Consulta -> Consulta -> Consulta) -> (Consulta -> Consulta -> Consulta) -> (Consulta -> Consulta -> Consulta) -> (Consulta -> Consulta -> Consulta) -> (Consulta -> Consulta -> Consulta) -> Consulta -> Consulta
foldConsulta fa ft fs fp fr fx fn fu fi fd (Taux t)    = fa t
foldConsulta fa ft fs fp fr fx fn fu fi fd (Tau t)     = ft t
foldConsulta fa ft fs fp fr fx fn fu fi fd (Sigma p q) = fs p (foldConsulta fa ft fs fp fr fx fn fu fi fd q)
foldConsulta fa ft fs fp fr fx fn fu fi fd (Pi cs q)   = fp cs (foldConsulta fa ft fs fp fr fx fn fu fi fd q)
foldConsulta fa ft fs fp fr fx fn fu fi fd (Rho t q)   = fr t (foldConsulta fa ft fs fp fr fx fn fu fi fd q)
foldConsulta fa ft fs fp fr fx fn fu fi fd (X q1 q2)   = fx (foldConsulta fa ft fs fp fr fx fn fu fi fd q1) (foldConsulta fa ft fs fp fr fx fn fu fi fd q2)
foldConsulta fa ft fs fp fr fx fn fu fi fd (NX q1 q2)  = fn (foldConsulta fa ft fs fp fr fx fn fu fi fd q1) (foldConsulta fa ft fs fp fr fx fn fu fi fd q2)
foldConsulta fa ft fs fp fr fx fn fu fi fd (U q1 q2)   = fu (foldConsulta fa ft fs fp fr fx fn fu fi fd q1) (foldConsulta fa ft fs fp fr fx fn fu fi fd q2)
foldConsulta fa ft fs fp fr fx fn fu fi fd (I q1 q2)   = fi (foldConsulta fa ft fs fp fr fx fn fu fi fd q1) (foldConsulta fa ft fs fp fr fx fn fu fi fd q2)
foldConsulta fa ft fs fp fr fx fn fu fi fd (D q1 q2)   = fd (foldConsulta fa ft fs fp fr fx fn fu fi fd q1) (foldConsulta fa ft fs fp fr fx fn fu fi fd q2)
