module Repl.Lexer where

import Text.ParserCombinators.ReadP
import Repl.Tokens( Token(..), Command(..) )
import Data.Char (isLetter)
import GHC.Unicode (isLower)
import Control.Applicative

commands = [
    (["t", "type"], CheckType []),
    (["n", "normalize"], Normalize),
    (["d", "desugar"], Desugar),
    (["q", "quit"], Quit),
    (["h", "?", "help"], Help)
    ]

tryCommand :: [([String], Command)] -> ReadP Command
tryCommand [] = pfail
tryCommand (([], cmd):xs) = tryCommand xs
tryCommand ((str:xstr, cmd):xs) = do
    do { string str; return cmd } <|> tryCommand ((xstr, cmd) : xs)

isLambda :: Char -> Bool
isLambda char = char `elem` "/\\λ"

isIdentifier :: Char -> Bool
-- todo: there has to be a better way
isIdentifier char = isLetter char && isLower char && not (isLambda char)

identifier :: ReadP Token
identifier = do
    letter <- count 1 (satisfy (\char -> isLetter char && not (isLambda char)))
    return $ Identifier letter

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

lexer :: ReadP [Token]
lexer =
    sepBy (choice [identifier, lambda, dot, leftParenthesis, rightParenthesis, colon, arrow, tau]) skipSpaces
