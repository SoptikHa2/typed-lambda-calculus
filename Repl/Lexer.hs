module Repl.Lexer where

import Text.ParserCombinators.ReadP
import Repl.Tokens( Token(..) )
import Core.Expression ( Expression(..) )

expression :: ReadP Expression
expression = error "unimplemented"

lambdaAbstraction :: ReadP Expression
lambdaAbstraction = error "unimplemented"

application :: ReadP Expression
application = do
    abstraction <- lambdaAbstraction
    value <- expression
    return $ Application abstraction value


variable :: ReadP Expression
variable = do
    skipSpaces 
    return $ Variable $ munch1 isLetter

