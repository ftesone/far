module PredicadoSimple
    (PredicadoSimple(..), foldPredicadoSimple)
where

import BOp
import Utils

type Atributo = String

data PredicadoSimple = Col BOp Atributo Atributo | Val BOp Atributo String

instance Show PredicadoSimple where
    show (Col bop c1 c2) = c1 ++ (show bop) ++ c2
    show (Val bop c v) = c ++ (show bop) ++ "\"" ++ (show v) ++ "\""

instance Read PredicadoSimple where
    readsPrec _ str =
        let
            c1 = trim $ takeWhile (not . flip elem "=≠>≥<≤") str
            c2 = trim $ (tail . dropWhile (not . flip elem "=≠>≥<≤")) str
            bop = read (filter (flip elem "=≠>≥<≤") str)
        in if elem '"' c1
            then [(Val (reversoBOp bop) c2 (filter (/='\"') c1), "")]
            else if elem '"' c2
                then [(Val bop c1 (filter (/='\"') c2), "")]
                else [(Col bop c1 c2, "")]

foldPredicadoSimple :: (BOp -> Atributo -> Atributo -> Bool) -> (BOp -> Atributo -> String -> Bool) -> PredicadoSimple -> Bool
foldPredicadoSimple fc fv (Col bop c1 c2) = fc bop c1 c2
foldPredicadoSimple fc fv (Val bop c v) = fv bop c v
