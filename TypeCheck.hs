module TypeCheck where

import Expression ( Expression(..) )
import Type ( Type(..) )

type Context = [(String, Type)]

type ExprResult = Either String Expression
type TypeResult = Either String Type

infer :: Context -> Expression -> Type
-- App
infer ctx (Application e e') =
    case infer ctx e of
        FunctionType t t' -> if check ctx e' t then t'
                             else error $ "Type does not match. expected " ++ show t ++ " @ " ++ show e' ++ " with context " ++ show ctx
        BaseType -> error $ "Expected function type, got base type @ " ++ show e
-- Var
infer ctx (Variable var) =
    case lookup var ctx of
        Just t -> t
        Nothing -> error $ "Failed to infer type of variable " ++ show var ++ " without further context. Annotate the variable."
-- Ann
infer ctx (AnnotatedExpression t e) = if check ctx e t then t
                                        else error $ "annotated type " ++ show t ++ " does not match"
infer ctx (LambdaAbstraction arg t body) =
    let ctx' = (arg, t) : ctx in
        FunctionType t (infer ctx' body)

check :: Context -> Expression -> Type -> Bool
-- Lam
check ctx (LambdaAbstraction arg at body) (FunctionType t t')
    | at == t = check ctx' body t'
    | otherwise = False
    where ctx' = (arg, t) : ctx
-- Chk
check ctx expr t = t == infer ctx expr
