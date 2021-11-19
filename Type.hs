module Type where

data Type -- τ
    = BaseType -- α
    | FunctionType Type Type -- τ→τ′ 
    deriving (Eq)

instance Show Type where
    show BaseType = "τ"
    show (FunctionType t1 t2) = "(" ++ show t1 ++ "→" ++ show t2 ++ ")"
