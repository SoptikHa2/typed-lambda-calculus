module Repl.Lexer where

import Text.ParserCombinators.ReadP
import Repl.Tokens( Token(..) )
import Data.Char (isLetter)
import GHC.Unicode (isLower)
import Control.Applicative

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

colon :: ReadP Token
colon = do
    char ':'
    return Colon

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
