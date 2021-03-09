module BOp
    (BOp(..), reversoBOp, resolverBOp)
where

data BOp = Eq | Ne | Gt | Ge | Lt | Le

instance Show BOp where
    show Eq = "="
    show Ne = "≠"
    show Gt = ">"
    show Ge = "≥"
    show Lt = "<"
    show Le = "≤"

instance Read BOp where
    readsPrec _ str = case head str of
        '=' -> [(Eq, tail str)]
        '≠' -> [(Ne, tail str)]
        '>' -> [(Gt, tail str)]
        '≥' -> [(Ge, tail str)]
        '<' -> [(Lt, tail str)]
        '≤' -> [(Le, tail str)]

reversoBOp :: BOp -> BOp
reversoBOp Eq = Eq
reversoBOp Ne = Ne
reversoBOp Gt = Lt
reversoBOp Ge = Le
reversoBOp Lt = Gt
reversoBOp Le = Ge

resolverBOp :: Ord a => BOp -> a -> a -> Bool
resolverBOp Eq = (==)
resolverBOp Ne = (/=)
resolverBOp Gt = (>)
resolverBOp Ge = (>=)
resolverBOp Lt = (<)
resolverBOp Le = (<=)
