module Core.Expression where

import Core.Type ( Type )


data Expression
    = Variable String
    | Application Expression Expression
    | LambdaAbstraction String Type Expression
    | AnnotatedExpression Type Expression
    deriving (Eq)

instance Show Expression where
    show (Variable var) = var
    show (Application a b) = show a ++ " " ++ show b
    show (LambdaAbstraction var t body) = "(λ" ++ var ++ ":" ++ show t ++ ". " ++ show body ++ ")"
    show (AnnotatedExpression t expr) = show expr ++ " :: " ++ show t
