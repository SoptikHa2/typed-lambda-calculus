module Core.Evaluation where

-- Linguistic note:
-- ┌───────────────┐
-- │ Zdragonlordit │
-- └───────────────┘
-- - dobrovolně si ztížit/zkomplikovat úkol/semestrálku/… tak, že by se z toho normální člověk pos*al
-- - splnit úkol „Vyrob a zapal ohniště“ (za předpokladu že je k dispozici vše co je potřeba pro výrobu
--   ohniště a zápalky) postavením malého osobního automobilu, který nikdy nepojede, a prohlášením ho za daleko lepší ohniště
-- - místo programu, který má vypsat celá čísla od jedné do pěti, sestavit menšího konkurenta mathematiky a pomocí něj dokázat,
--   že 1, 2, 3, 4 a 5 jsou opravdu celá čísla. Pokud postup u čísla čtyři selže, přesto toto řešení obhajovat jako jediný správný postup

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

-- Rename all free variables and lambda arguments using gien replacement rules
renameFreeVariables :: Expression -> ReplacementRules -> Expression
renameFreeVariables (Variable var) rr =
    case lookup var rr of
        Just expr -> expr
        Nothing -> Variable var
renameFreeVariables (LambdaAbstraction arg t body) rr =
    case lookup arg rr of
        Just (Variable arg') -> (LambdaAbstraction arg' t (renameFreeVariables body rr))
        Nothing -> LambdaAbstraction arg t (renameFreeVariables body rr)
renameFreeVariables (Application left right) rr =
    Application (renameFreeVariables left rr) (renameFreeVariables right rr)
renameFreeVariables (AnnotatedExpression t expr) rr =
    AnnotatedExpression t (renameFreeVariables expr rr)

-- Returns true when expression contains string in any form, be it
-- expression body or function argument
containsFreeVariable :: Expression -> String -> Bool
containsFreeVariable (Variable var) query
    | var == query = True
    | otherwise    = False
containsFreeVariable (LambdaAbstraction arg _ body) query
    | arg == query = False
    | otherwise    = containsFreeVariable body query
containsFreeVariable (Application left right) query =
    containsFreeVariable left query || containsFreeVariable right query
containsFreeVariable (AnnotatedExpression _ expr) query =
    containsFreeVariable expr query

-- Returns all names that might potentially cause name conflict, be it
-- expression body or function argument
getConflictingNames :: Expression -> [String]
getConflictingNames (Variable var) = [var]
getConflictingNames (Application left right) = getConflictingNames left ++ getConflictingNames right
getConflictingNames (LambdaAbstraction arg _ body) = arg : getConflictingNames body
getConflictingNames (AnnotatedExpression _ expr) = getConflictingNames expr

-- Alpha-convert lambda abstraction (Left) until there are no collisions with Right
alphaConversion :: Expression -> Expression -> Expression
alphaConversion (LambdaAbstraction arg t body) right
    | any (\var -> containsFreeVariable right var) conflictingNames =
        alphaConversion (LambdaAbstraction (newArg arg) t (renameFreeVariables body rr)) right
    | otherwise = (LambdaAbstraction arg t body)
    where newArg = (\arg -> arg ++ "'")
          conflictingNames = arg : getConflictingNames body
          rr = map (\x -> (x, Variable (newArg x))) conflictingNames
alphaConversion expr right = error "Alpha conversion received non-lambda-abstraction."

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
                    normalize alphaConvertedBody rr'
                    where --rr' = (v, normalize right rr) : rr
                          alphaConvertedExpr = alphaConversion left right
                          alphaConvertedBody = case alphaConvertedExpr of
                            LambdaAbstraction _ _ body -> body
                          rr' = case alphaConvertedExpr of
                            LambdaAbstraction v' _ _ -> (v', normalize right rr) : rr
                _ -> Application (normalize left rr) (normalize right rr)
        AnnotatedExpression t expr -> normalize expr rr
