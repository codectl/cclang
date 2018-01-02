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

-- Applies token to the current state resulting in a different state
evalToken :: [Macro] -> State -> Token -> State
evalToken macros state token = state

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
    loadMacros n (l:ls)
      | n == 0    = []
      | otherwise = readMacro l : loadMacros (n-1) ls
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
getMacroTokens [] _ = error "Macro not found"
getMacroTokens ((name, _, tokens):macros) lookup_name = if name == lookup_name then tokens else getMacroTokens macros lookup_name

-- Get macro arity by name
getMacroArity :: [Macro] -> Name -> Arity
getMacroArity [] _ = error "Macro not found"
getMacroArity ((name, arity, _):macros) lookup_name = if name == lookup_name then arity else getMacroArity macros lookup_name

-- Get variable value by name
getVar :: [Variable] -> Name -> Value
getVar [] _ = error "Variable not found"
getVar ((name, value):variables) lookup_name = if name == lookup_name then value else getVar variables lookup_name

-- Set variable value by name
setVar :: [Variable] -> Name -> Value -> [Variable]
setVar [] _ _ = error "Variable not found"
setVar ((name, value):variables) lookup_name replace_value
  | name == lookup_name = (name, replace_value):variables
  | otherwise           = (name, value) : setVar variables lookup_name replace_value