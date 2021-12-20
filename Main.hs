module Main where

import Text.ParserCombinators.ReadP
import System.IO
import Core.Expression
import Core.TypeCheck (infer)
import Repl.Parser (readUserInput, ParserResult (..))
import Control.Applicative
import Core.Type (Type(Unspecified))
import Repl.Tokens (Command(..))
import Core.Evaluation
import Core.Desugaring

main :: IO ()
main = repl Nothing

-- Print repl to screen, with given expression, if there is any.
-- If there is any expression given, all commands executed will be ran on it.
repl :: Maybe Expression -> IO ()
repl e = do
    -- Print saved expression if there is any
    case e of
        Just expr -> putStrLn $ printColoredParens expr 0
        _ -> putStrLn "Enter lambda expression or :h for help"
    input <- prompt "\x1b[1;92mλ-> >>\x1b[0m "
    let cmd = readUserInput input
    case cmd of
        -- New expression from user
        Expression ex -> repl $ Just ex
        -- Desugar and then normalize the expression, if there is one
        Command (Normalize n) -> case e of
            Just e' -> repl $ Just $ normalizeResult e'
                where normalizeResult = flip normalizeTimes n . desugar
            _ -> putStrLn "Cannot normalize invalid input"
        Command FullNormalize -> case e of
            Just e' -> repl $ Just $ fullNormalizeResult e'
                where fullNormalizeResult = normalizeUntilNormal . desugar
            _ -> putStrLn "Cannot normalize invalid input"
        -- Desugar the expression and save the result
        Command Desugar -> case e of
            Just e' -> repl $ Just $ desugar e'
            _ -> putStrLn "Cannot desugar invalid input"
        -- Type annotate (and check) expression, user may provide additional context
        Command (CheckType ctx) -> case e of
            Just e' -> case infer ctx e' of
                Left err -> putStrLn err
                Right t -> repl $ Just (AnnotatedExpression t e')
            _ -> putStrLn "Cannot check type of invalid input"
        -- :q
        Command Quit -> putStrLn "bye"
        -- :a
        Command (Apply expr) -> case e of
            Just e' -> repl $ Just $ Application e' expr
            _ -> putStrLn "Cannot apply expression to nothing"
        -- :h
        Command Help -> do
            putStrLn ":n(ormalize) N -> desugar and normalize expression (N steps, default=1)"
            putStrLn ":f(ullNormalize), :c(ontinue) -> normalize expression until it is fully normalized. Might not stop ever!"
            putStrLn ":d(esugar) -> desugar expression"
            putStrLn ":t(ype) [x:t, y:(t->t)] -> type check expression with given variable context"
            putStrLn ":a(pply) <EXPRESSION> -> apply given expression to current one"
            putStrLn ":q(uit)"
        Invalid -> putStrLn "Failed to parse input"
    repl e

prompt :: String -> IO String
prompt msg = do
    putStr msg
    hFlush stdout
    getLine
