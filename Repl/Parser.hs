module Repl.Parser where

import Repl.Tokens (Token(..), Command (CheckType))
import Repl.Lexer
import Core.Expression(Expression(..))
import Core.Type(Type(..))
import Text.ParserCombinators.ReadP
import Control.Applicative
import Core.TypeCheck (Context)

data ParserResult
    = Expression Expression
    | Command Command
    | Invalid
    deriving Show

baseType :: ReadP Type
baseType = do
    skipSpaces
    tau
    return BaseType

functionType :: ReadP Type
functionType = do
    skipSpaces
    leftParenthesis
    skipSpaces
    t1 <- functionType <|> baseType
    skipSpaces
    arrow
    skipSpaces
    t2 <- functionType <|> baseType
    skipSpaces
    rightParenthesis
    return (FunctionType t1 t2)

typeAnnotation :: ReadP Type
typeAnnotation = do
    skipSpaces
    colon
    skipSpaces
    functionType <|> baseType

variable :: ReadP Expression
variable = do
    skipSpaces
    Identifier ident <- identifier
    return (Variable ident)

lambdaAbstraction :: ReadP Expression
lambdaAbstraction = do
    skipSpaces
    leftParenthesis
    lambda
    Identifier param <- identifier
    annotatedType <- typeAnnotation <|> return Unspecified
    skipSpaces
    dot
    body <- expression
    skipSpaces
    rightParenthesis
    return (LambdaAbstraction param annotatedType body)

application :: ReadP Expression
application = do
    lambda <- lambdaAbstraction <|> variable <|> parenthesisedExpression
    param <- expression
    return (Application lambda param)

parenthesisedExpression :: ReadP Expression
parenthesisedExpression = do
    skipSpaces
    leftParenthesis
    exp <- expression
    skipSpaces
    rightParenthesis
    return exp

expression :: ReadP Expression
expression = do
    expr <- choice [parenthesisedExpression, variable, application, lambdaAbstraction]
    t <- typeAnnotation <|> return Unspecified
    if t == Unspecified then return expr else return (AnnotatedExpression t expr)

nextContext :: ReadP Context
nextContext = do
    skipSpaces
    comma
    context

context :: ReadP Context
context = do
    skipSpaces
    Identifier var <- identifier
    t <- typeAnnotation
    afterContext <- nextContext <|> return []
    return $ (var, t) : afterContext

typeCommand :: ReadP Command
typeCommand = do
    tryCommand [(["t", "type"], CheckType [])]
    skipSpaces
    leftSquareParenthesis 
    ctx <- context
    skipSpaces
    rightSquareParenthesis
    return $ CheckType ctx

command :: ReadP Command
command = do
    skipSpaces
    colon
    skipSpaces
    typeCommand <|> tryCommand commands

readUserInput :: String -> ParserResult
readUserInput str = case exprResult of
    [] -> if null commandResult then Invalid else Command (extract commandResult)
    _ -> Expression (extract exprResult)
    where
        exprResult = readP_to_S expression str
        commandResult = readP_to_S command str
        extract = fst . last
