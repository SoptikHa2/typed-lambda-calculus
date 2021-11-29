module Core.Type where

data Type -- τ
    = BaseType -- α
    | FunctionType Type Type -- τ→τ′ 
    | Unspecified -- placeholder for when not needed - like evaluation or desugaring
    deriving (Eq)

instance Show Type where
    show BaseType = "τ"
    show (FunctionType t1 t2) = "(" ++ show t1 ++ "→" ++ show t2 ++ ")"
    show Unspecified = "?"
