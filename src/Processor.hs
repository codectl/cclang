module Processor where

import Stack
import System.IO
import Data.List

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

-- Supported boolean operators
booleanOperators :: [(String, (Integer -> Integer -> Bool))]
booleanOperators =  [("!", (!)), ("&", (&)), ("|", (\|))]
  where
    (&) a b = True
    (!) a b = True
    (\|) a b = True

lookupOperator :: String -> [(String, (a -> a -> a))] -> (a -> a -> a)
lookupOperator _ [] = error "operator not found"
lookupOperator x ((k,v):os) = if x == k then v else lookupOperator x os

-- Applies token to the current state resulting in a different state
evalToken :: [Macro] -> State -> Token -> State
evalToken macros (vars, Stack stack, out) token
  | token == ","                      = (vars, snd . evalPop $ Stack stack, out ++ (show . fst . evalPop $ Stack stack) ++ [' '])
  | token == "."                      = (vars, snd . evalPop $ Stack stack, out ++ (show . fst . evalPop $ Stack stack) ++ ['\n'])
  | elem token $ words "+ - * / %"    = (vars, evalState token arithmeticOperators $ Stack stack, out) -- arithmetic tokens
  | elem token $ words "< <= > >= ==" = (vars, evalState token relationalOperators $ Stack stack, out) -- relational tokens
  | token !! 0 == '@'                 = case pop $ Stack stack of
                                       (Nothing, _)             -> error "peeking empty stack"
                                       (Just top, Stack stack') -> (setVar vars (['@'] ++ token) top, Stack stack', out) -- variable case
  | otherwise                         = (vars, push (read token :: Integer) $ Stack stack, out)
  where
    evalState token operators (Stack stack) = push (evalExpression token operators $ Stack stack) (snd . evalPop . snd . evalPop $ Stack stack)
    evalExpression token operators (Stack stack) = (lookupOperator token operators) (fst . evalPop . snd . evalPop $ Stack stack) (fst . evalPop $ Stack stack)
    evalPop (Stack stack) = case pop $ Stack stack of
      (Nothing, _)           -> error "poping empty stack"
      (Just x, Stack stack') -> (x, Stack stack')


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
getMacroTokens :: [Macro] -> Name -> Maybe [Token]
getMacroTokens [] _ = Nothing
getMacroTokens ((name, _, tokens):macros) lookup_name = if name == lookup_name then Just tokens else getMacroTokens macros lookup_name

-- Get macro arity by name
getMacroArity :: [Macro] -> Name -> Maybe Arity
getMacroArity [] _ = Nothing
getMacroArity ((name, arity, _):macros) lookup_name = if name == lookup_name then Just arity else getMacroArity macros lookup_name

-- Get variable value by name
getVar :: [Variable] -> Name -> Maybe Value
getVar [] _ = Nothing
getVar ((name, value):variables) lookup_name = if name == lookup_name then Just value else getVar variables lookup_name

-- Set variable value by name
setVar :: [Variable] -> Name -> Value -> [Variable]
setVar [] name value = [(name, value)]
setVar ((name, value):variables) lookup_name replace_value
  | name == lookup_name = (name, replace_value):variables
  | otherwise           = (name, value) : setVar variables lookup_name replace_value