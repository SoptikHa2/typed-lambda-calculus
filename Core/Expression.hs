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
    show (Application a b) = "(" ++ show a ++ " " ++ show b ++ ")"
    show (LambdaAbstraction var t body) = "(λ" ++ var ++ ":" ++ show t ++ ". " ++ show body ++ ")"
    show (AnnotatedExpression t expr) = show expr ++ " :: " ++ show t

printInColor :: Char -> Integer -> String
printInColor c 0 = [c]
printInColor c n = "\x1b[3" ++ show (n `mod` 7 + 1) ++ "m" ++ [c] ++ "\x1b[0m"

-- Print expression, but in a colored way, depending on nest level. Starts at zero.
printColoredParens :: Expression -> Integer -> String
printColoredParens (Variable var) _ = var
printColoredParens (Application a b) n =
    printInColor '(' n ++ printColoredParens a (n+1) ++ " " ++ printColoredParens b (n+1) ++ printInColor ')' n
printColoredParens (LambdaAbstraction var t body) n =
    printInColor '(' n ++ "λ" ++ var ++ ":" ++ show t ++ ". " ++ printColoredParens body (n+1) ++ printInColor ')' n
printColoredParens (AnnotatedExpression t expr) n =
    printColoredParens expr n ++ " :: " ++ show t
