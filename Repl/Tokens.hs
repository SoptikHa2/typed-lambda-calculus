module Repl.Tokens where
import Core.TypeCheck (Context)

data Token
  = Identifier String
  | Lambda
  | Dot
  | LeftParenthesis
  | RightParenthesis
  | LeftSquareParenthesis
  | RightSquareParenthesis
  | Colon
  | Comma
  | Arrow
  | Tau
  deriving Show

data Command
  = CheckType Context
  | Desugar
  | Normalize Integer -- How many steps should be executed at once?
  | FullNormalize
  | Quit
  | Help
  deriving Show