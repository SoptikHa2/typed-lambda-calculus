module Core.Desugaring where

import Core.Evaluation (normalize)
import Core.Expression(Expression(..))
import Core.Type(Type(..))
import Repl.Parser(expression, expressionFromStr)
import Text.Read(readMaybe)
import Control.Applicative

type DesugarRules = [(String, Expression)]

_desugarRules :: DesugarRules
_desugarRules = [
        ("Y", convert "(λ f . (λ x . f (x x)) (λ x . f (x x)))"),
        ("ZERO", convert "(λ n . n (λ x . (λ t f . f)) (λ t f . t))"),
        ("PRED", convert "(λ x s z . x (λ f g . g (f s)) (λ g . z) (λ u . u))"),
        ("SUC", convert "(λ n s z . s (n s z))"),
        ("AND", convert "(λ x y . x y x)"),
        ("OR", convert "(λ x y . x T y)"),
        ("T", convert "(λ t f . t)"),
        ("F", convert "(λ t f . f)"),
        ("NOT", convert "(λ x t f . x f t)"),
        ("PLUS", convert "(λ x y s z . x s (y s z))"),
        ("MINUS", convert "(λ m n . (n PRED) m)"),
        ("TIMES", convert "(λ x y s . x (y s))"),
        ("DIV", convert "(λ n . Y (λ c n m f x . (λ d . (ZERO) d (0 f x) (f (c d m f x))) ((MINUS) n m)) ((SUC) n))"),
        ("EXP", convert "(λ x y . y x)"),
        ("DELTA", convert "(λ m n . (PLUS) ((MINUS) m n) ((MINUS) n m))"),
        ("EQ", convert "(λ m n . (ZERO) ((DELTA) m n))"),
        ("GT", convert "(λ m n . (NOT) ((ZERO) ((MINUS) m n)))"),
        ("LT", convert "(λ m n . (GT) n m )"),
        ("GE", convert "(λ m n . (ZERO) ((MINUS) n m))"),
        ("LE", convert "(λ m n . (ZERO) ((MINUS) m n))"),
        ("0", convert "(λ s z . z)")
    ]
    where convert = fst . last . expressionFromStr

generateNumFromSUC :: Int -> Expression
generateNumFromSUC 0 = fst . last . expressionFromStr $ "0"
generateNumFromSUC n  = Application (convert "SUC") (generateNumFromSUC (n-1))
    where convert = fst . last . expressionFromStr

desugarNumber :: String -> Maybe Expression
desugarNumber str = generateNumFromSUC <$> (readMaybe str)

desugar :: Expression -> Expression
-- Variable stays the same, unless it's overriden by desugar rules
desugar (Variable v) = case (lookup v _desugarRules) <|> (desugarNumber v) of

    Just expr -> desugar expr
    Nothing -> Variable v
-- Application, LambdaAbstraction and AnnotatedExpressions stay the same
desugar (Application fnc ap) = Application (desugar fnc) (desugar ap)
desugar (LambdaAbstraction v t expr) = LambdaAbstraction v t (desugar expr)
desugar (AnnotatedExpression t expr) = AnnotatedExpression t (desugar expr)
