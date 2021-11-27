module Core.Evaluation where

import Core.Expression ( Expression(..) ) 

type ReplacementRules = [(String, Expression)]

normalize :: Expression -> ReplacementRules -> Expression 
normalize (Variable v) rr = case lookup v rr of
    Just expr -> expr
    Nothing -> Variable v
normalize (Application (LambdaAbstraction param t expr) apl) rr = let rr' = (param, apl) : rr in
    normalize expr rr'
normalize (Application (AnnotatedExpression _ e) apl) rr = normalize (Application (normalize e rr) apl) rr
normalize (Application expr apl) rr = case normalize expr rr of
  LambdaAbstraction param t expr' -> normalize (Application (LambdaAbstraction param t expr') apl) rr
  expr' -> error $ "Left hand of applicatio doesn't contain lambda abstraction even after normalization. Received " ++ show expr'
normalize (LambdaAbstraction param t expr) rr = let rr' = (param, Variable param) : rr in
    LambdaAbstraction param t $ normalize expr rr'
normalize (AnnotatedExpression _ e) rr = normalize e rr
