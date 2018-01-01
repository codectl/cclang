module Processor where

import Stack
import System.IO

-- Types
type Name = String
type Arity = Int
type Token = String
type Macro = (Name, Arity, [Token])

type Value = Integer
type Variable = (Name, Value)

type State = ([Variable], Stack Integer, String)
type Program = ([Macro], State, [Token])


--evalToken :: [Macro] -> State -> Token -> State
--
--eval :: Program -> State
--
--processData :: String -> IO State
--

-- Read commands from text file and executes
run :: FilePath -> IO ()
run filepath = do
  contents <- readFile filepath
  let allLines = lines contents
      result = filter (\line -> head line /= '#') allLines
    in putStrLn $ unlines $ result

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