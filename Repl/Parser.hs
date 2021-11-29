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

-- Load one single Tau
baseType :: ReadP Type
baseType = do
    skipSpaces
    tau
    return BaseType

-- Load type function. HAS TO BE parenthesised (TODO)
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

-- Read type annotation (including the :)
typeAnnotation :: ReadP Type
typeAnnotation = do
    skipSpaces
    colon
    skipSpaces
    functionType <|> baseType

-- Read one identifier
variable :: ReadP Expression
variable = do
    skipSpaces
    Identifier ident <- identifier
    return (Variable ident)

-- Parse lambda abstraction, enclosed in parenthesis
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

-- Parse application. First expression must be enclosed in parenthesis, unless it's a variable
application :: ReadP Expression
application = do
    lambda <- lambdaAbstraction <|> variable <|> parenthesisedExpression
    param <- expression
    return (Application lambda param)

-- Read any expression inside one extra set of parenthesis
parenthesisedExpression :: ReadP Expression
parenthesisedExpression = do
    skipSpaces
    leftParenthesis
    exp <- expression
    skipSpaces
    rightParenthesis
    return exp

-- Read any expression
expression :: ReadP Expression
expression = do
    expr <- choice [parenthesisedExpression, variable, application, lambdaAbstraction]
    t <- typeAnnotation <|> return Unspecified
    if t == Unspecified then return expr else return (AnnotatedExpression t expr)

-- Load one more element of typechecking context
nextContext :: ReadP Context
nextContext = do
    skipSpaces
    comma
    context

-- Load typechecking context in format [x:t, y:(t->t)]
context :: ReadP Context
context = do
    skipSpaces
    Identifier var <- identifier
    t <- typeAnnotation
    afterContext <- nextContext <|> return []
    return $ (var, t) : afterContext

-- Load type command with argument
typeCommand :: ReadP Command
typeCommand = do
    tryCommand [(["t", "type"], CheckType [])]
    skipSpaces
    leftSquareParenthesis 
    ctx <- context
    skipSpaces
    rightSquareParenthesis
    return $ CheckType ctx

-- Load any supported command, as defined in Lexer or here
command :: ReadP Command
command = do
    skipSpaces
    colon
    skipSpaces
    typeCommand <|> tryCommand commands

-- Read expression or command from user (string) input
readUserInput :: String -> ParserResult
readUserInput str = case exprResult of
    [] -> if null commandResult then Invalid else Command (extract commandResult)
    _ -> Expression (extract exprResult)
    where
        exprResult = readP_to_S expression str
        commandResult = readP_to_S command str
        extract = fst . last
