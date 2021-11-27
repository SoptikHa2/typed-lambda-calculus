module Core.Desugaring where

import Core.Evaluation (normalize)
import Core.Expression(Expression(..))
import Core.Type(Type(..))

type DesugarRules = [(String, Expression)]


_desugarRules :: DesugarRules
_desugarRules = [
        ("T", LambdaAbstraction "t" BaseType (LambdaAbstraction "f" BaseType (Variable "t")))
    ]

desugar :: Expression -> Expression
desugar (Variable v) = case lookup v _desugarRules of
    Just expr -> expr
    Nothing -> Variable v
desugar (Application fnc ap) = Application (desugar fnc) (desugar ap)
desugar (LambdaAbstraction v t expr) = LambdaAbstraction v t (desugar expr)
desugar (AnnotatedExpression t expr) = AnnotatedExpression t (desugar expr)
