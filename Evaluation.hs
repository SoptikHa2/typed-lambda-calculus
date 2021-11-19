module Evaluation where

import Expression ( Expression(..) ) 

type ReplacementRules = [(String, Expression)]

_replacementRule :: String -> ReplacementRules -> Maybe Expression
_replacementRule _ [] = Nothing 
_replacementRule var ((key, expr):xs)
    | var == key = Just expr
    | otherwise = _replacementRule var xs

normalize :: Expression -> ReplacementRules -> Expression 
normalize (Variable v) rr = case _replacementRule v rr of
    Just expr -> expr
    Nothing -> Variable v
normalize (Application (LambdaAbstraction param t expr) apl) rr = let rr' = (param, apl) : rr in
    normalize expr rr'
normalize (Application (AnnotatedExpression _ e) apl) rr = normalize (Application (normalize e rr) apl) rr
normalize (Application _ _) _ = error "Application requires lambda expression as it's first parameter"
normalize (LambdaAbstraction param t expr) rr = LambdaAbstraction param t $ normalize expr rr
normalize (AnnotatedExpression _ e) rr = normalize e rr
