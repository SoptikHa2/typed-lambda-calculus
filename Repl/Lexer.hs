module Repl.Lexer where

import Text.ParserCombinators.ReadP
import Repl.Tokens( Token(..), Command(..) )
import Data.Char (isLetter, isNumber)
import GHC.Unicode (isLower)
import Control.Applicative

commands = [
    (["t", "type"], CheckType []),
    (["n", "normalize"], Normalize 1),
    (["d", "desugar"], Desugar),
    (["q", "quit"], Quit),
    (["h", "?", "help"], Help)
    ]

-- Try to parse any command from the list above.
-- Note, that this doesn't parse command arguments, and will
-- use defaults, as specified in list above
tryCommand :: [([String], Command)] -> ReadP Command
tryCommand [] = pfail
tryCommand (([], cmd):xs) = tryCommand xs
tryCommand ((str:xstr, cmd):xs) = do
    do { string str; return cmd } <|> tryCommand ((xstr, cmd) : xs)

isLambda :: Char -> Bool
isLambda char = char `elem` "/\\λ"

isIdentifierChar :: Char -> Bool
isIdentifierChar char = (isLetter char || isNumber char) && not (isLambda char)

identifierChar :: ReadP Char
identifierChar = satisfy isIdentifierChar

identifier :: ReadP Token
identifier = do
    name <- many1 identifierChar
    return $ Identifier name

singleCharacterIdentifier :: ReadP Token
singleCharacterIdentifier = do
    name <- satisfy isIdentifierChar
    return $ Identifier [name]

lambda :: ReadP Token
lambda = do
    satisfy isLambda
    return Lambda

dot :: ReadP Token
dot = do
    char '.'
    return Dot

leftParenthesis :: ReadP Token
leftParenthesis = do
    char '('
    return LeftParenthesis

rightParenthesis :: ReadP Token
rightParenthesis = do
    char ')'
    return RightParenthesis

leftSquareParenthesis :: ReadP Token
leftSquareParenthesis = do
    char '['
    return LeftSquareParenthesis

rightSquareParenthesis :: ReadP Token
rightSquareParenthesis = do
    char ']'
    return RightSquareParenthesis

colon :: ReadP Token
colon = do
    char ':'
    return Colon

comma :: ReadP Token
comma = do
    char ','
    return Comma

arrow :: ReadP Token
arrow = do
    string "->"
    return Arrow

tau :: ReadP Token
tau = do
    char 't' <|> char 'T' <|> char 'τ'
    return Tau

number :: ReadP Integer
number = do
    str <- munch1 isNumber
    return $ read str