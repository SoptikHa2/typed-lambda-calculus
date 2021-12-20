module Core.Evaluation where

import Core.Expression ( Expression(..) )
import Data.Maybe
import Debug.Trace

type ReplacementRules = [(String, Expression)]
type ExprResult = Either String Expression

-- Normalize expression, until it is in normal form
normalizeUntilNormal :: Expression -> Expression
normalizeUntilNormal expr
    | not $ isInNormalForm expr = normalizeUntilNormal (normalize expr)
    | otherwise = expr

-- Execute normalization step multiple times
normalizeTimes :: Expression -> Integer -> Expression
normalizeTimes expr 0 = expr
normalizeTimes expr n =
    normalizeTimes (normalize expr) (n-1)

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

-- Get names of all free variables in given expression
getFreeVariables :: Expression -> [String]
getFreeVariables expr = getFreeVariables' expr []
  where getFreeVariables' :: Expression -> [String] -> [String]
        getFreeVariables' (Variable x) nops
          | elem x nops = []
          | otherwise   = [x]
        getFreeVariables' (LambdaAbstraction arg _ body) nops =
            getFreeVariables' body (arg : nops)
        getFreeVariables' (Application left right) nops =
            getFreeVariables' left nops ++ getFreeVariables' right nops
        getFreeVariables' (AnnotatedExpression _ aexp) nops =
            getFreeVariables' aexp nops

-- Rename all free variables using given replacement rules
renameFreeVariables :: Expression -> ReplacementRules -> Expression
renameFreeVariables (Variable var) rr =
    case lookup var rr of
        Just expr -> expr
        Nothing -> Variable var
renameFreeVariables (LambdaAbstraction arg t body) rr =
    LambdaAbstraction arg t (renameFreeVariables body rr')
        where rr' = (arg, Variable arg) : rr -- shadowing
renameFreeVariables (Application left right) rr =
    Application (renameFreeVariables left rr) (renameFreeVariables right rr)
renameFreeVariables (AnnotatedExpression t expr) rr =
    AnnotatedExpression t (renameFreeVariables expr rr)

-- Alpha-convert given expression, if needed. Variable names that the expression might collide with
-- is provided as array of strings.
alphaConversion :: Expression -> [String] -> Expression
alphaConversion expr rules =
    case expr of
        LambdaAbstraction arg t body -> 
            if elem arg rules then
                alphaConversion (LambdaAbstraction (genNewName arg) t (renameFreeVariables body [(arg, Variable (genNewName arg))])) rules
            else 
                LambdaAbstraction arg t (alphaConversion body rules)
        Application left right -> Application (alphaConversion left rules) (alphaConversion right rules)
        AnnotatedExpression t aexp -> AnnotatedExpression t (alphaConversion aexp rules)
        _ -> expr
    where genNewName = (\arg -> '\'' : arg)

-- Beta-convert expression using some replacement rules
betaConversion :: Expression -> ReplacementRules -> Expression
betaConversion expr rr =
    case expr of
        Variable arg -> fromMaybe expr (lookup arg rr)
        LambdaAbstraction arg t body ->
            LambdaAbstraction arg t (betaConversion body ((arg, Variable arg) : rr))
        Application left right ->
            Application (betaConversion left rr) (betaConversion right rr)
        AnnotatedExpression _ aexp -> normalize aexp

-- Execute one stpe of normalization using set of replacement rules
normalize :: Expression -> Expression
normalize expr =
    case expr of
        LambdaAbstraction arg t body -> LambdaAbstraction arg t (normalize body)
        Application left right ->
            case left of
                (LambdaAbstraction arg t body) ->
                    betaConversion (alphaConversion body (getFreeVariables right)) [(arg, right)]
                _ -> Application (normalize left) (normalize right)
        AnnotatedExpression _ aexp -> normalize aexp
        _ -> expr
