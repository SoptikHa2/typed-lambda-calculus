module Repl.Parser where

import Repl.Tokens (Token(..))
import Repl.Lexer
import Core.Expression(Expression(..))
import Core.Type(Type(..))
import Text.ParserCombinators.ReadP
import Control.Applicative

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
