module Main where
import Lexer (lexer)
import MyError (withExit)
import Parser (parser)
import Evaluate (evaluator, createMap)
import System.Environment (getArgs)
import Interpreter (repl)
import Type ( Token, Expression, Defined)
import GHC.Base (returnIO)

parseArg :: [String] -> Maybe ([String], Bool)
parseArg ("-h":xs) = Nothing
parseArg ("-i" : xs) = Just (reverse xs, True)
parseArg xs = Just (reverse xs, False)

help :: String
help = "USAGE: ./hal file [...] [-i]\n\tfile\tLisp file\t-i\tUse interpreter"

halI :: [String] -> IO ()
halI files = mapM readFile files >>= repl . snd . evaluator . parser . lexer . concat

halFPrintRes :: ([Expression], Defined) -> IO()
halFPrintRes ([], defined) = pure ()
halFPrintRes (res, defined) = print $ last res

halF :: [String] -> IO()
halF files = mapM readFile files >>= halFPrintRes . evaluator . parser . lexer . concat

-- halF' files = mapM readFile files >>= print . parser . lexer . concat

main :: IO ()
main = withExit $ do
        argv <- getArgs
        let arg = parseArg $ reverse argv in
                case arg of
                        Nothing -> print help
                        Just (xs, False) -> halF xs
                        Just (xs, True) -> halI xs
