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
main = do
    repl Nothing

repl :: Maybe Expression -> IO ()
repl e = do
    case e of
        Just expr -> print expr
        _ -> putStrLn "Enter lambda expression or :h for help"
    input <- prompt "λ-> >> "
    let cmd = readUserInput input
    case cmd of
       Expression ex -> repl $ Just ex
       Command Normalize -> case e of
            Just e' -> case normalizeResult e' of
                    Left err -> putStrLn err
                    Right expr -> repl $ Just expr
                where normalizeResult = flip normalize [] . desugar
            _ -> putStrLn "Cannot normalize invalid input"
       Command Desugar -> case e of
            Just e' -> repl $ Just $ desugar e'
            _ -> putStrLn "Cannot desugar invalid input"
       Command CheckType -> case e of
            Just e' -> case infer [] e' of
                Left err -> putStrLn err
                Right t -> repl $ Just (AnnotatedExpression t e')
            _ -> putStrLn "Cannot check type of invalid input"
       Command Quit -> putStrLn "bye"
       Command Help -> do
            putStrLn ":n(ormalize) -> desugar and normalize expression"
            putStrLn ":d(esugar) -> desugar expression"
            putStrLn ":t(ype) [x:t, y:(t->t)] -> type check expression with given variable context"
            putStrLn ":q(uit)"
       Invalid -> putStrLn "Failed to parse input"
    repl e

prompt :: String -> IO String
prompt msg = do
    putStr msg
    hFlush stdout
    getLine