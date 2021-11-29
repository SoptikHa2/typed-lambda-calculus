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
  | Desugar
  | Normalize
  | Quit
  | Help
  deriving Show