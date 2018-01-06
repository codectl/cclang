module Processor where

import Stack
import System.IO
import Data.List
import Data.Bits

-- Types
type Name = String
type Arity = Int
type Token = String
type Macro = (Name, Arity, [Token])

type Value = Integer
type Variable = (Name, Value)

type State = ([Variable], Stack Integer, String)
type Program = ([Macro], State, [Token])

-- Supported arithmetic operators
arithmeticOperators :: [(String, (Integer -> Integer -> Integer))]
arithmeticOperators = [("+", (+)), ("-", (-)), ("*", (*)), ("/", div), ("%", (%))]
  where (%) a b = mod a b

-- Supported relational operators
relationalOperators :: [(String, (Integer -> Integer -> Integer))]
relationalOperators = [("<", (lt)), ("<=", (le)), (">", (gt)), (">=", (ge)), ("==", (eq))]
  where
    (lt) a b = if a < b then 1 else 0
    (le) a b = if a <= b then 1 else 0
    (gt) a b = if a > b then 1 else 0
    (ge) a b = if a >= b then 1 else 0
    (eq) a b = if a == b then 1 else 0

-- Supported bitwise operators
bitwiseOperators :: [(String, (Integer -> Integer -> Integer))]
bitwiseOperators =  [("&", (.&.)), ("|", (.|.))]

-- Numeric operators
numericOperators :: (Num a) => [(String, a)]
numericOperators = arithmeticOperators ++ relationalOperators ++ bitwiseOperators ++ singleArgOperators

-- Single argument operators
singleArgOperators :: [(String, (Integer -> Integer))]
singleArgOperators = [(("!"), (!))]
  where (!) a = if a == 0 then 1 else 0

-- Double argument operators
doubleArgOperators :: [(String, (Integer -> Integer -> Integer))]
doubleArgOperators = numericOperators

-- Stack operators
stackOperators :: [(String, a)]
stackOperators = [("dup", dup), ("swap", swap), ("peek", peek), ("pop", evalSinglePop), ("size", size), ("nil", nil)]

-- Lookup operator given its textual representation
lookupOperator :: String -> a
lookupOperator token
  | belongsOperator token numericOperators = getOperator token numericOperators
  | belongsOperator token stackOperators = getOperator token stackOperators
   where
     getOperator _ [val] = snd val
     getOperator token (t:ts) = if fst t == token then snd t else getOperator token ts

-- Lists all supported operators
supportedOperators :: [String]
supportedOperators = getOperatorsName singleArgOperators ++ getOperatorsName doubleArgOperators ++ getOperatorsName stackOperators

-- Gets the arity for the operator
arityOperator :: String -> Arity
arityOperator token
  | belongsOperator token singleArgOperators = 1
  | belongsOperator token doubleArgOperators = 2
  | otherwise = error "operator not found"

-- Gather all names from the given operator list
getOperatorsName :: [(String, a)] -> [String]
getOperatorsName operators = map (\t -> fst t) operators

-- Checks whether operator belongs to the given operator list
belongsOperator :: String -> [(String, a)] -> Bool
belongsOperator token operators = elem token $ getOperatorsName operators

-- Applies token to the current state resulting in a different state
evalToken :: [Macro] -> State -> Token -> State
evalToken macros (vars, Stack stack, out) token
  | token == ","                      = (vars, snd . evalSinglePop $ Stack stack, out ++ (show . fst . evalSinglePop $ Stack stack) ++ [' '])
  | token == "."                      = (vars, snd . evalSinglePop $ Stack stack, out ++ (show . fst . evalSinglePop $ Stack stack) ++ ['\n'])
  | elem token supportedOperators     = (vars, evalState token supportedOperators $ Stack stack, out)
  | token !! 0 == '@'                 = (setVar vars (['@'] ++ token) $ fst . evalSinglePop $ Stack stack, snd . evalSinglePop $ Stack stack, out)
  | otherwise                         = (vars, push (read token :: Integer) $ Stack stack, out)
  where
    evalState token operators (Stack stack)
      | belongsOperator token numericOperators = push (evalExpression token operators $ Stack stack) (Stack $ drop (arityOperator token) stack)
      | belongsOperator token stackOperators   = (Stack stack)

evalExpression token operators (Stack stack)
  | belongsOperator token singleArgOperators = (lookupOperator token) (fst . evalSinglePop $ Stack stack)
  | belongsOperator token doubleArgOperators = (lookupOperator token) (fst . evalPop 2 $ Stack stack) (fst . evalSinglePop $ Stack stack)

evalPop :: Int -> Stack Integer -> (Integer, Stack Integer)
evalPop n (Stack stack) = case pop $ Stack $ drop (n-1) stack of
  (Nothing, _)      -> error "poping empty stack"
  (Just x, Stack stack') -> (x, Stack stack')

evalSinglePop :: Stack Integer -> (Integer, Stack Integer)
evalSinglePop (Stack stack) = evalPop 1 $ Stack stack

-- Computes the final state of a program from a starting point state
eval :: Program -> State
eval (_, state, []) = state
eval (macros, state, (token:tokens)) = eval (macros, evalToken macros state token, tokens)

-- Builds up program based on file data and returns result from eval
processData :: String -> IO State
processData rawFileData = return $ eval (macros, initialState, tokens)
  where
    tokens = words $ unlines $ drop (numMacros + 1) fileData
    initialState = ([], Stack [], "")
    macros = loadMacros numMacros (tail fileData)
    numMacros = read $ head fileData
    loadMacros n (l:ls) = if n == 0 then [] else readMacro l : loadMacros (n-1) ls
    fileData = removeComments $ lines $ rawFileData
    removeComments fd = filter (\line -> head line /= '#') fd

-- Reads data from text file and outputs result from processData
run :: FilePath -> IO ()
run filepath = do
  contents <- readFile filepath
  (_, _, output) <- processData $ contents
  putStrLn output

-- Reading a macro from string
readMacro :: String -> Macro
readMacro line = readMacro' $ words line
  where
    readMacro' ls = ((['$'] ++ head ls), (read $ head $ tail ls), (drop 2 ls))

-- Get macro token by name
getMacroTokens :: [Macro] -> Name -> [Token]
getMacroTokens [] _ = error "no macro found"
getMacroTokens ((name, _, tokens):macros) lookup_name = if name == lookup_name then tokens else getMacroTokens macros lookup_name

-- Get macro arity by name
getMacroArity :: [Macro] -> Name -> Arity
getMacroArity [] _ = error "no macro found"
getMacroArity ((name, arity, _):macros) lookup_name = if name == lookup_name then arity else getMacroArity macros lookup_name

-- Get variable value by name
getVar :: [Variable] -> Name -> Value
getVar [] _ = error "no variable found"
getVar ((name, value):variables) lookup_name = if name == lookup_name then value else getVar variables lookup_name

-- Set variable value by name
setVar :: [Variable] -> Name -> Value -> [Variable]
setVar [] name value = [(name, value)]
setVar ((name, value):variables) lookup_name replace_value
  | name == lookup_name = (name, replace_value):variables
  | otherwise           = (name, value) : setVar variables lookup_name replace_value