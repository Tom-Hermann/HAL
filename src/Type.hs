--
-- EPITECH PROJECT, 2021
-- B-FUN-501-RUN-5-1-HAL-tom.hermann
-- File description:
-- Type
--

module Type(Defined(..), Function(..), Expression(..), Parsing(..), Keyword(..), Token(..)) where
import Data.Map.Strict as Map (Map)
import Foreign (Int64, Bits (xor))


type Defined = Map.Map Token Expression

data Function = Defined [String] Parsing | Builtin ([Expression] -> Expression) | Spe ([Parsing] -> Defined -> Expression)
data Expression = Function Function | Number Int64 | ESymbol String | List [Expression] | EState Bool | Nil

instance Eq Expression where
    Number x == Number y = x == y
    ESymbol x == ESymbol y = x == y
    List x == List y = x == y
    EState x == EState y = x == y
    Function (Defined s e) == Function (Defined sx ex) = s == sx && e == ex
    Nil == Nil = True
    _ == _ = False

instance Show Expression where
    show (Function x) = "#<procedure>"
    show (Number x) = show x
    show (ESymbol x) = x
    show (EState x)
        | x = "#t"
        | otherwise  = "#f"
    show (List [a]) = "(" ++ show a ++ ")"
    show (List [a, b]) = "(" ++ show a ++ " . " ++ show b ++ ")"
    show (List [a, b, Nil]) = "(" ++ show a ++ " . " ++ show b ++ ")"
    show (List []) = "()"
    show (List (x:xs)) = "(" ++ showBigList (x:xs)
    show Nil = "()"

showBigList :: [Expression] -> String
showBigList [Nil] = ")"
showBigList [x] = show x  ++ ")"
showBigList [x, Nil] = show x  ++ ")"
showBigList (x:xs) = show x ++ " " ++ showBigList xs
showBigList [] = ")"

data Parsing = Elem Token | Quoted Parsing | Expr [Parsing] deriving(Show, Eq, Ord)


data Keyword = Cons
            | Car
            | Cdr
            | Eq
            | Sup
            | Inf
            | Atom
            | Cond
            | BTrue
            | BFalse
            | Add
            | Sub
            | Mul
            | Div
            | Mod
            | Quote
            | Lambda
            | Define
            | Let
            | OpenP
            | CloseP
            deriving (Eq,  Ord)

instance Show Keyword where
    show Cons = "Cons"
    show Car = "Car"
    show Cdr = "Cdr"
    show Eq = "Eq?"
    show Sup = ">"
    show Inf = "<"
    show Atom = "Atom"
    show Cond = "Cond"
    show BTrue = "#t"
    show BFalse = "#f"
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "div"
    show Mod = "Mod"
    show Quote = ""
    show Lambda = "lambda"
    show Define = "define"
    show Let = "let"
    show x = ""


data Token = Key Keyword | Nb Int64 | Symbol String | State Bool
            deriving (Eq, Ord)

instance Show Token where
    show (Key x) = show x
    show (Nb x) = show x
    show (Symbol x) = x
    show (State True) = "#t"
    show (State False) = "#f"

