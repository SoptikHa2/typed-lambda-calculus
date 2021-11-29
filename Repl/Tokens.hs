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

data Command
  = CheckType
  | Normalize
  | Quit
  deriving Show