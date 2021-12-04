--
-- EPITECH PROJECT, 2021
-- B-FUN-501-RUN-5-1-HAL-tom.hermann
-- File description:
-- Parser
--

module Lexer (lexer) where
import Foreign (Int64)
import Type(Keyword(..), Token(..))
import Data.Char (isDigit, Char, isSpace)
import Data.List.NonEmpty (cons)
import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import MyError (throwMyError)


isDelimiter :: Char -> Bool
isDelimiter '('  = True
isDelimiter ')'  = True
isDelimiter '\'' = True
isDelimiter '\n' = True
isDelimiter chr  = isSpace chr

isolateElem :: String -> (String, String)
isolateElem = break isDelimiter

getKeywordOperator :: String -> Maybe Token
getKeywordOperator "+"  = Just (Key Add)
getKeywordOperator "-"  = Just (Key Sub)
getKeywordOperator "*"  = Just (Key Mul)
getKeywordOperator "div"  = Just (Key Div)
getKeywordOperator "mod"  = Just (Key Mod)
getKeywordOperator _ = Nothing

getKeywordBoolean :: String -> Maybe Token
getKeywordBoolean "eq?"  = Just (Key Eq)
getKeywordBoolean "atom?"  = Just (Key Atom)
getKeywordBoolean "cond"  = Just (Key Cond)
getKeywordBoolean "#t"  = Just (State True)
getKeywordBoolean "#f"  = Just (State False)
getKeywordBoolean "<"  = Just (Key Inf)
getKeywordBoolean ">"  = Just (Key Sup)
getKeywordBoolean _ = Nothing

getOtherKeyWord :: String -> Token
getOtherKeyWord string = let value = getKeywordBoolean string in
    case value of
        Nothing -> let value = getKeywordOperator string in
            case value of
                Nothing -> Symbol string
                Just operator -> operator
        Just boolean -> boolean

getKeyword :: String -> Token
getKeyword "cons" = Key Cons
getKeyword "car" = Key Car
getKeyword "cdr" = Key Cdr
getKeyword "quote" = Key Quote
getKeyword "lambda" = Key Lambda
getKeyword "define" = Key Define
getKeyword "let" = Key Let
getKeyword string = getOtherKeyWord string


getNumber :: String -> Int64
getNumber number = let maybeNumber = readMaybe number :: Maybe Int64 in
                    case maybeNumber of
                        Just realNumber -> realNumber
                        _ -> throwMyError "Float not supported"


parserToken :: String -> [Token]
parserToken [] = []
parserToken (x:xs) = let (elem, save) = isolateElem (x:xs) in
                case elem of
                    [] -> []
                    (x:xs) -> case isDigit x of
                            True  -> Nb (getNumber elem): lexer save
                            _ -> getKeyword elem:lexer save


lexer :: String -> [Token]
lexer (' ':xs) = lexer xs
lexer ('\t':xs) = lexer xs
lexer ('\r':xs) = lexer xs
lexer ('\n':xs) = lexer xs
lexer ('(':xs) = Key OpenP:lexer xs
lexer ('\'':xs) = Key Quote:lexer xs
lexer (')':xs) = Key CloseP:lexer xs
lexer string = parserToken string