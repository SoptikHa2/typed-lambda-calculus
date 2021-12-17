module Core.Evaluation where

import Core.Expression ( Expression(..) )
import Data.Maybe

type ReplacementRules = [(String, Expression)]
type ExprResult = Either String Expression

-- Normalize expression, until it is in normal form
normalizeUntilNormal :: Expression -> Expression
normalizeUntilNormal expr
    | not $ isInNormalForm expr = normalizeUntilNormal (normalize expr [])
    | otherwise = expr

-- Execute normalization step multiple times
normalizeTimes :: Expression -> Integer -> Expression
normalizeTimes expr 0 = expr
normalizeTimes expr n =
    normalizeTimes (normalize expr []) (n-1)

-- Is in normal form?
--  Expression is in normal form, iff
--      - is a variable
--      - body of lambda abstraction is in normal form
--      - left and right side of an application are both in a normal form
isInNormalForm :: Expression -> Bool
isInNormalForm expr =
    case expr of
        LambdaAbstraction _ _ body -> isInNormalForm body
        Application (LambdaAbstraction _ _ _) _ -> False
        Application left right -> isInNormalForm left && isInNormalForm right
        _ -> True

-- Execute one stpe of normalization using set of replacement rules
normalize :: Expression -> ReplacementRules -> Expression
normalize expr rr =
    case expr of
        Variable arg -> fromMaybe expr (lookup arg rr)
        LambdaAbstraction arg t body -> LambdaAbstraction arg t (normalize body rr')
            where rr' = (arg, Variable arg) : rr
        Application left right ->
            case left of
                -- Application
                LambdaAbstraction v _ body ->
                    normalize body rr'
                    where rr' = (v, normalize right rr) : rr
                _ -> Application (normalize left rr) (normalize right rr)
        AnnotatedExpression t expr -> normalize expr rr
