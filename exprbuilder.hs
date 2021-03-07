module ExprBuilder
    (createExprBuilder, setExpr, addExpr, addUnaryOp, addBinaryOp, getExpr)
where

data ExprBuilder a = Empty | Built a | Unary (a -> a)

createExprBuilder = Empty

setExpr _ q = Built q

addExpr :: ExprBuilder a -> a -> ExprBuilder a
addExpr (Empty) q = Built q
addExpr (Unary u) q = Built $ u q

addUnaryOp :: ExprBuilder a -> (a -> a) -> ExprBuilder a
addUnaryOp (Empty) u = Unary u
addUnaryOp (Unary q) u = Unary $ q . u

addBinaryOp :: ExprBuilder a -> (a -> a -> a) -> ExprBuilder a
addBinaryOp (Built q) b = Unary $ b q

getExpr :: ExprBuilder a -> a
getExpr (Built q) = q
