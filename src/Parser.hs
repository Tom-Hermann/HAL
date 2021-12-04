--
-- EPITECH PROJECT, 2021
-- B-FUN-501-RUN-5-1-HAL-tom.hermann
-- File description:
-- parser
--

module Parser (parser) where

import Type(Token(..), Keyword(..), Parsing(..))
import MyError(throwMyError)

collectExpr'' :: [Parsing] -> (Parsing, [Token]) -> ([Parsing], [Token])
collectExpr'' list (expr, tokens) = (expr:list, tokens)

collectExpr' :: ([Parsing], [Token]) -> (Parsing, [Token])
collectExpr' (expr, Key CloseP:xs) = (Expr (reverse expr), xs)
collectExpr' (expr, Key OpenP:xs) =
                            collectExpr' $ collectExpr'' expr (collectExpr xs)
collectExpr' (expr, Key Quote :xs) = let (exprXs, rest) = parserParsing xs in
                    collectExpr' $ collectExpr'' expr (Quoted exprXs, rest)
collectExpr' (expr, x:xs) = collectExpr' (Elem x:expr,xs)
collectExpr' (_, [])   = throwMyError "Unmatched opening parenthesis"

collectExpr :: [Token] -> (Parsing, [Token])
collectExpr tokens = collectExpr' ([], tokens)

parserParsing :: [Token] -> (Parsing, [Token])
parserParsing (Key OpenP:xs) = collectExpr xs
parserParsing (Key Quote:xs) = let (expr, rest) = parserParsing xs in
                        (Quoted expr, rest)
parserParsing (Key CloseP: xs) = throwMyError "Unmatched closing parenthesis"
parserParsing [] = throwMyError "Empty expression"
parserParsing (x:xs) = (Elem x, xs)

parser :: [Token] -> [Parsing]
parser [] = []
parser tokens = let (expr, save) = parserParsing tokens in expr:parser save