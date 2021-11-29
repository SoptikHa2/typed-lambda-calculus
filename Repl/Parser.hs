module Repl.Parser where

import Repl.Tokens (Token(..), Command (CheckType, Normalize))
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

-- Read one identifier, typed in upper case. This should be prioritized
upperCaseVariable :: ReadP Expression
upperCaseVariable = do
    skipSpaces
    Identifier ident <- upperCaseIdentifier
    return (Variable ident)

-- Construct multiple nested lambda abstractions form one with multiple arguments
constructLambdaAbstraction :: [(String, Type)] -> Expression -> Expression
constructLambdaAbstraction ((e, t):xs) body = LambdaAbstraction e t (constructLambdaAbstraction xs body)
constructLambdaAbstraction _ body = body

lambdaParameter :: ReadP (String, Type)
lambdaParameter = do
    skipSpaces
    Identifier i <- singleCharacterIdentifier 
    t <- typeAnnotation <|> return Unspecified
    return (i, t)

-- Parse lambda abstraction, enclosed in parenthesis. Allows multiple arguments.
lambdaAbstraction :: ReadP Expression
lambdaAbstraction = do
    skipSpaces
    lambda
    parameters <- many1 lambdaParameter
    skipSpaces
    dot
    body <- expression
    return $ constructLambdaAbstraction parameters body

-- Parse application. First expression must be enclosed in parenthesis, unless it's a variable
application :: ReadP Expression
application = do
    lambda <- lambdaAbstraction <++ variable <++ parenthesisedExpression
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
    expr <- upperCaseVariable <++ application <++ lambdaAbstraction <++ variable <++ parenthesisedExpression
    t <- typeAnnotation <|> return Unspecified
    if t == Unspecified then return expr else return (AnnotatedExpression t expr)

expressionFromStr :: ReadS Expression
expressionFromStr = readP_to_S expression

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
typeCommandWithArgument :: ReadP Command
typeCommandWithArgument = do
    tryCommand [(["t", "type"], CheckType [])]
    skipSpaces
    leftSquareParenthesis
    ctx <- context
    skipSpaces
    rightSquareParenthesis
    return $ CheckType ctx

normalizeCommandWithArgument :: ReadP Command
normalizeCommandWithArgument = do
    tryCommand [(["n", "normalize"], Normalize 1)]
    skipSpaces
    times <- number
    return $ Normalize times

-- Load any supported command, as defined in Lexer or here
command :: ReadP Command
command = do
    skipSpaces
    colon
    skipSpaces
    typeCommandWithArgument <|> normalizeCommandWithArgument <|> tryCommand commands

-- Read expression or command from user (string) input
readUserInput :: String -> ParserResult
readUserInput str = case exprResult of
    [] -> if null commandResult then Invalid else Command (extract commandResult)
    _ -> Expression (extract exprResult)
    where
        exprResult = readP_to_S expression str
        commandResult = readP_to_S command str
        extract = fst . last
