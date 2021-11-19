module Expression where

import Type ( Type )


data Expression
    = Variable String
    | Application Expression Expression
    | LambdaAbstraction String Expression
    | AnnotatedLambdaAbstraction String Type Expression
    | AnnotatedExpression Type Expression
    deriving (Eq)

instance Show Expression where
    show (Variable var) = var
    show (Application a b) = show a ++ " " ++ show b
    show (LambdaAbstraction var body) = "(λ" ++ var ++ ". " ++ show body ++ ")"
    show (AnnotatedLambdaAbstraction var t body) = "(λ" ++ var ++ ":" ++ show t ++ ". " ++ show body ++ ")"
    show (AnnotatedExpression t expr) = show expr ++ " :: " ++ show t
