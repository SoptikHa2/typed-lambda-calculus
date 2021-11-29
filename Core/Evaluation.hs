module Core.Evaluation where

import Core.Expression ( Expression(..) )
import Data.Maybe

type ReplacementRules = [(String, Expression)]
type ExprResult = Either String Expression

isInNormalForm :: Expression -> Bool
isInNormalForm expr =
    case expr of
        LambdaAbstraction _ _ body -> isInNormalForm body
        Application (LambdaAbstraction _ _ _) _ -> False
        Application left right -> isInNormalForm left && isInNormalForm right
        _ -> True

normalize :: Expression -> ReplacementRules -> Expression
normalize expr rr =
    case expr of
        Variable arg -> fromMaybe expr (lookup arg rr)
        LambdaAbstraction arg t body -> LambdaAbstraction arg t (normalize body rr)
        Application left right ->
            case left of
                LambdaAbstraction v _ body ->
                    normalize body rr'
                    where rr' = (v, normalize right rr) : rr
                _ -> Application (normalize left rr) (normalize right rr)
        AnnotatedExpression t expr -> normalize expr rr