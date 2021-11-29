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
-- Variable stays the same, unless it's overriden by desugar rules
desugar (Variable v) = case lookup v _desugarRules of
    Just expr -> expr
    Nothing -> Variable v
-- Application, LambdaAbstraction and AnnotatedExpressions stay the same
desugar (Application fnc ap) = Application (desugar fnc) (desugar ap)
desugar (LambdaAbstraction v t expr) = LambdaAbstraction v t (desugar expr)
desugar (AnnotatedExpression t expr) = AnnotatedExpression t (desugar expr)
