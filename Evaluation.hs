module Evaluation where

import Expression ( Expression(..) ) 

type ReplacementRules = [(String, Expression)]

normalize :: Expression -> ReplacementRules -> Expression 
normalize (Variable v) rr = case lookup v rr of
    Just expr -> expr
    Nothing -> Variable v
normalize (Application (LambdaAbstraction param t expr) apl) rr = let rr' = (param, apl) : rr in
    normalize expr rr'
normalize (Application (AnnotatedExpression _ e) apl) rr = normalize (Application (normalize e rr) apl) rr
normalize (Application _ _) _ = error "Application requires lambda expression as it's first parameter"
normalize (LambdaAbstraction param t expr) rr = LambdaAbstraction param t $ normalize expr rr
normalize (AnnotatedExpression _ e) rr = normalize e rr
normalize exp _ = error $ "Application didn't recognize expression " ++ show exp ++ ". Try desugaring first."