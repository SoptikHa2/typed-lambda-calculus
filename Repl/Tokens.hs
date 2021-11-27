module Repl.Tokens where

data Token
  = Identifier String
  | Lambda
  | Dot
  | LeftParenthesis
  | RightParenthesis
  | Colon
  | Arrow
  | Tau
  deriving Show
