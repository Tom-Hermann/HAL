--
-- EPITECH PROJECT, 2021
-- B-FUN-501-RUN-5-1-HAL-tom.hermann
-- File description:
-- Interpreter
--

module Interpreter (repl) where
import Type (Defined(..))
import Control.Exception ( catch )
import MyError (TerminationException(TE))
import Lexer (lexer)
import Parser (parser)
import Evaluate (evaluatorR)
import System.Console.Haskeline
    ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT )

replError' :: String -> Defined -> IO ()
replError' message defined = putStrLn ("*** ERROR : " ++ message ++ ".")
                    >> repl defined

replError :: IO () -> Defined -> IO ()
replError action defined = action `catch`
                            (\(TE code message) -> replError' message defined)

repl'' :: Defined -> Maybe String -> InputT IO()
repl'' defined Nothing = return ()
repl'' defined (Just "quit") = return ()
repl'' defined (Just line) = let (value, newdDefined) = evaluatorR (parser $ lexer line) defined in
                        case value of
                            [] -> repl' newdDefined
                            _ -> mapM_ (outputStrLn . show) value >> repl' newdDefined

repl' :: Defined -> InputT IO()
repl' defined = getInputLine "> " >>= repl'' defined

repl :: Defined -> IO ()
repl defined = replError (runInputT defaultSettings (repl' defined)) defined


