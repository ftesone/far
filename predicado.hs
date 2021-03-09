module Predicado
    (Predicado(..), resolverPredicado)
where

import BOp
import PredicadoSimple
import ExprBuilder
import Utils

type Atributo = String

data Predicado = Simple PredicadoSimple | Not Predicado | Or Predicado Predicado | And Predicado Predicado

instance Show Predicado where
    show (Simple p) = show p
    show (Not p) = "~ (" ++ (show p) ++ ")"
    show (Or p1 p2) = "(" ++ (show p1) ++ ") ∨ ("++ (show p2) ++ ")"
    show (And p1 p2) = "(" ++ (show p1) ++ ") ∧ ("++ (show p2) ++ ")"

instance Read Predicado where
    readsPrec _ str =
        let
            readPredicado (pb,[]) = (pb,[])
            readPredicado (pb,('(':cs)) = readPredicado (addExpr pb $ getExpr $ fst $ readPredicado (createExprBuilder, fst $ parens ('(':cs)), trim $ snd $ parens ('(':cs))
            readPredicado (pb,('∨':cs)) = readPredicado (addBinaryOp pb Or, trim cs)
            readPredicado (pb,('∧':cs)) = readPredicado (addBinaryOp pb And, trim cs)
            readPredicado (pb,('~':cs)) = readPredicado (addUnaryOp pb Not, trim cs)
            readPredicado (pb,s) =
                let w = takeWhile (not . flip elem "~∨∧") s
                in if length w > 1
                    then readPredicado (addExpr pb $ Simple $ (read  w :: PredicadoSimple), trim $ drop (length w) s)
                    else (addExpr pb $ Simple $ (read w :: PredicadoSimple), "")
        in [(getExpr $ fst $ readPredicado (createExprBuilder, fixSpaces str), "")]

foldPredicado :: (PredicadoSimple -> Bool) -> (Bool -> Bool) -> (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Predicado -> Bool
foldPredicado fp fn fo fa (Simple p) = fp p
foldPredicado fp fn fo fa (Not p) = fn (foldPredicado fp fn fo fa p)
foldPredicado fp fn fo fa (Or p1 p2) = fo (foldPredicado fp fn fo fa p1) (foldPredicado fp fn fo fa p2)
foldPredicado fp fn fo fa (And p1 p2) = fa (foldPredicado fp fn fo fa p1) (foldPredicado fp fn fo fa p2)

resolverPredicado :: (BOp -> Atributo -> Atributo -> Bool) -> (BOp -> Atributo -> String -> Bool) -> Predicado -> Bool
resolverPredicado fc fv = foldPredicado (foldPredicadoSimple fc fv) (not) (||) (&&)
