module BOp
    (BOp(..), resolverBOp)
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

resolverBOp :: Ord a => BOp -> a -> a -> Bool
resolverBOp Eq = (==)
resolverBOp Ne = (/=)
resolverBOp Gt = (>)
resolverBOp Ge = (>=)
resolverBOp Lt = (<)
resolverBOp Le = (<=)
