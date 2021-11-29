module Core.TypeCheck where

import Core.Expression ( Expression(..) )
import Core.Type ( Type(..) )

type Context = [(String, Type)]

type TypeResult = Either String Type

infer :: Context -> Expression -> TypeResult
-- App
infer ctx (Application e e') =
    case infer ctx e of
        Right (FunctionType t t') -> if check ctx e' t then Right t'
                             else Left $ "Type does not match. expected '" ++ show t ++ "' @ " ++ show e' ++ " with context " ++ show ctx
        Right otherType -> Left $ "Expected function type, got '" ++ show otherType ++ "' @ " ++ show e
        Left s -> Left s
-- Var
infer ctx (Variable var) =
    case lookup var ctx of
        Just t -> Right t
        Nothing -> Left $ "Failed to infer type of variable " ++ show var ++ " without further context. Annotate the variable."
-- Ann
infer ctx (AnnotatedExpression t e) = if check ctx e t then Right t
                                        else Left $ "Annotated type '" ++ show t ++ "' does not match"
infer ctx (LambdaAbstraction arg t body) = do
    let ctx' = (arg, t) : ctx
    t' <- infer ctx' body
    Right (FunctionType t t')

check :: Context -> Expression -> Type -> Bool
-- Lam
check ctx (LambdaAbstraction arg at body) (FunctionType t t')
    | at == t = check ctx' body t'
    | otherwise = False
    where ctx' = (arg, t) : ctx
-- Chk
check ctx expr t = Right t == infer ctx expr
