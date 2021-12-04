--
-- EPITECH PROJECT, 2021
-- B-FUN-501-RUN-5-1-HAL-tom.hermann
-- File description:
-- Evaluator
--

module Evaluate (evaluator, evaluatorR, createMap) where
import Foreign (Int64)
import Type
    ( Parsing(..),
      Token(..),
      Keyword(..),
      Expression(..),
      Defined,
      Function(..) )
import MyError (throwMyError)
import Data.Map as Map (fromList, insert, lookup, null)


elemToExpression :: Token -> Expression
elemToExpression (Nb x) = Number x
elemToExpression (Symbol x) = ESymbol x
elemToExpression (State x) = EState x
elemToExpression x = ESymbol $ show x

elemToFunct :: Token -> Defined -> Expression
elemToFunct key defined = let retFunct = Map.lookup key defined in
                                case retFunct of
                                    Nothing -> elemToExpression key
                                    Just x -> x

createVar :: [String] -> [Expression] -> Defined -> Defined
createVar (var:varxs) (value:valuexs) defined =
                createVar varxs valuexs (Map.insert (Symbol var) value defined)
createVar [] [] defined = defined
createVar _ _ defined =
                    throwMyError "Defined function: Unvalid number of argument"


callFunct :: Function -> [Expression] -> Defined -> Expression
callFunct (Builtin funct) args defined = funct args
callFunct (Defined args funct) values defined =
                        evaluateParsing funct (createVar args values defined)
callFunct _ _ _= Nil

evaluateExpr' :: Expression -> [Parsing] -> Defined -> Expression
evaluateExpr' (Function (Spe function)) expr defined = function expr defined
evaluateExpr'  value expr defined =
                    let values = value:map (`evaluateParsing` defined) expr in
                        case values of
                            (Function funct : xs) -> callFunct funct xs defined
                            [] -> Nil
                            _ -> throwMyError "Expression isn't a function:"


evaluateExpr :: [Parsing] -> Defined -> Expression
evaluateExpr (expr:next) defined =
                evaluateExpr' (evaluateParsing expr defined) next defined
evaluateExpr [] defined = Nil

evaluateExprQ :: [Parsing] -> [Expression]
evaluateExprQ (x:xs) = evaluateQuoted x:evaluateExprQ xs
evaluateExprQ [] = [Nil]

evaluateQuoted :: Parsing -> Expression
evaluateQuoted (Elem x) = elemToExpression x
evaluateQuoted (Quoted x) = evaluateQuoted x
evaluateQuoted (Expr []) = Nil
evaluateQuoted (Expr x) = List (evaluateExprQ x)

evaluateParsing :: Parsing -> Defined -> Expression
evaluateParsing (Elem x) defined = elemToFunct x defined
evaluateParsing (Expr x) defined = evaluateExpr x defined
evaluateParsing (Quoted x) defined = evaluateQuoted x

newFunct :: [Parsing] -> Parsing -> Expression
newFunct argv core = Function (Defined (map asStringP argv) core)

evaluateDefine :: [Parsing] -> Defined -> Defined
evaluateDefine [Elem (Symbol name), value] defined =
                Map.insert (Symbol name) (evaluateParsing value defined) defined
evaluateDefine [Expr (Elem (Symbol name):argv), funct] defined =
                Map.insert (Symbol name) (newFunct argv funct) defined
evaluateDefine _ defined = throwMyError "Define: Invalid form"

evaluator' :: [Parsing] -> Defined -> ([Expression], Defined)
evaluator' (Expr (Elem (Key Define) : funct) : xs) defined =
                                    evaluator' xs (evaluateDefine funct defined)
evaluator' (x:xs) defined = (evaluateParsing x defined:values, newDefined)
                    where
                        (values, newDefined) = evaluator' xs defined
evaluator' [] defined = ([], defined)

evaluator :: [Parsing] -> ([Expression], Defined)
evaluator x = evaluator' x createMap

evaluatorR :: [Parsing] -> Defined -> ([Expression], Defined)
evaluatorR x defined
    | Map.null defined = evaluator' x createMap
    | otherwise = evaluator' x defined



--                                      Defined
--As
asNum :: Expression -> Int64
asNum (Number x) = x
asNum error = throwMyError(show error ++ " must be a number")

asStringP :: Parsing -> String
asStringP (Elem  (Symbol string)) = string
asStringP error = throwMyError(show error ++ " must be a string")

asString :: Expression -> String
asString (ESymbol x) = x
asString error = throwMyError(show error ++ " must be a string")

asState :: Expression -> Bool
asState (EState x) = x
asState error = throwMyError(show error ++ " must be a bool")

asList :: Expression -> [Expression]
asList (List x) = x
asList error = throwMyError(show error ++ " must be a list")
-- As

--Add
add :: [Expression] -> Expression
add value = Number $ sum $ map asNum value
--

-- Sub
subList :: [Int64] -> Int64
subList [x] = x
subList (x:xs:xss) = subList (x - xs:xss)
subList [] = 0

sub :: [Expression] -> Expression
sub [value] = Number $ - asNum value
sub value = Number $ subList $ map asNum value
--

--Mul
mul :: [Expression] -> Expression
mul value = Number $ product $ map asNum value
--

--Div
sdiv :: [Expression] -> Expression
sdiv [x,y] = Number $ quot (asNum x) (asNum y)
sdiv _ = throwMyError "Div: Invalid argument: Need two"
--

--Mod
smod :: [Expression] -> Expression
smod [x,y] = Number $ asNum x `mod` asNum y
smod _ = throwMyError "Mod: Invalid argument: Need two"
--

--Inf
inf :: [Expression] -> Expression
inf [x,y] = EState $ asNum x < asNum y
inf _ = throwMyError "Inf: Invalid argument: Need two"
--

--Sup
sup :: [Expression] -> Expression
sup [x,y] = EState $ asNum x > asNum y
sup _ = throwMyError "Sup: Invalid argument: Need two"
--

--Cons
cons :: [Expression] -> Expression
cons [List x, Nil] = List x
cons [x, List xs] = List $ x:xs
cons [x, xs] = List [x, xs]
cons _ = throwMyError "Cons: Invalid argument: Need two"
--

--Car
car :: [Expression] -> Expression
car [List(x:xs)] = x
car _ = throwMyError "Car: Invalid argument"
--

--cdr
cdr :: [Expression] -> Expression
cdr [List [x]] = throwMyError "Cdr: Invalid argument"
cdr [List(x:xs)] = List xs
cdr _ = throwMyError "Cdr: Invalid argument"
--

--eq
eq :: [Expression] -> Expression
eq [x,xs] = EState (x == xs)
eq _ = throwMyError "Eq: Invalid argument"
--

--atom
atom :: [Expression] -> Expression
atom [List _] = EState False
atom _ = EState True
--

--Quote
quote :: [Parsing] -> Defined -> Expression
quote [expr] defined = evaluateQuoted expr
quote _ defined = throwMyError "Quote: Invalid argument"
--

--Cond
cond' ::  Expression  -> Parsing -> [Parsing] -> Defined -> Expression
cond' (EState False) ret xs defined = cond xs defined
cond' (EState True)  ret xs defined = evaluateParsing ret defined
cond' _ ret xs defined = throwMyError error
    where
        error = "Cond : Error syntax (Need a least one segment #t)"

cond :: [Parsing] -> Defined -> Expression
cond (Expr [expr, ret] : xs) defined =
                            cond' (evaluateParsing expr defined) ret xs defined
cond _ defined = throwMyError "Cond : Error syntax"
--

-- lamba
lambda' :: Parsing -> Parsing -> Expression
lambda' (Expr args) funct = Function (Defined (map asStringP args) funct)
lambda' error funct = throwMyError "Lambda: Error syntax"

lambda :: [Parsing] -> Defined -> Expression
lambda [args, values] defined = lambda' args values
lambda error defined = throwMyError "Lambda: Error syntax"
--

--let
--


createDefined :: [(Token , Expression)]
createDefined = [(Key Add, Function (Builtin add)),
                (Key Sub, Function (Builtin sub)),
                (Key Div, Function (Builtin sdiv)),
                (Key Mul, Function (Builtin mul)),
                (Key Inf, Function (Builtin inf)),
                (Key Sup, Function (Builtin sup)),
                (Key Mod, Function (Builtin smod)),
                (Key Cons, Function (Builtin cons)),
                (Key Car, Function (Builtin car)),
                (Key Cdr, Function (Builtin cdr)),
                (Key Eq, Function (Builtin eq)),
                (Key Atom, Function (Builtin atom)),
                (Key Quote, Function (Spe quote)),
                (Key Cond, Function (Spe cond)),
                (Key Lambda, Function (Spe lambda))
                ]

createMap :: Defined
createMap = Map.fromList createDefined