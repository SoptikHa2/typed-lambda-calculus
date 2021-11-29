module Core.Evaluation where

import Core.Expression ( Expression(..) ) 

type ReplacementRules = [(String, Expression)]
type ExprResult = Either String Expression

normalize :: Expression -> ReplacementRules -> ExprResult 
-- Variable remains same, unless it should be replaced (expl. during application)
normalize (Variable v) rr = case lookup v rr of
    Just expr -> Right expr
    Nothing -> Right $ Variable v
-- Application just normalizes function body, while replacing function parameter
normalize (Application (LambdaAbstraction param t expr) apl) rr = let rr' = (param, apl) : rr in
    normalize expr rr'
-- Ignore annotated expressions during applications
normalize (Application (AnnotatedExpression _ e) apl) rr = do
    bodyResult <- normalize e rr
    normalize (Application bodyResult apl) rr
-- When application first parameter is not lambda abstraction, try to normalize it, or fail
normalize (Application expr apl) rr = case normalize expr rr of
  Right (LambdaAbstraction param t expr') -> normalize (Application (LambdaAbstraction param t expr') apl) rr
  Right expr' -> Left $ "Left hand of application doesn't contain lambda abstraction even after normalization. Received " ++ show expr'
  Left s -> Left s
-- Normalize lambda abstraction's body
normalize (LambdaAbstraction param t expr) rr = do
    let rr' = (param, Variable param) : rr
    body <- normalize expr rr'
    Right $ LambdaAbstraction param t body
-- Ignore annotated expressions
normalize (AnnotatedExpression _ e) rr = normalize e rr
