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


--evalToken :: [Macro] -> State -> Token -> State


eval :: Program -> State
eval _ = ([], Stack [], "")

processData :: String -> IO ()
processData rawFileData = print $ evalAll programs
  where
    evalAll [] = []
    evalAll (p:ps) = eval (macros, initialState, words p) : evalAll ps
    programs = drop (numMacros + 1) fileData
    initialState = ([], Stack [], "")
    macros = loadMacros numMacros (tail fileData)
    numMacros = read $ head fileData
    loadMacros n (l:ls)
      | n == 0    = []
      | otherwise = readMacro l : loadMacros (n-1) ls
    fileData = removeComments $ lines $ rawFileData
    removeComments fd = filter (\line -> head line /= '#') fd

-- Read commands from text file and executes
run :: FilePath -> IO ()
run filepath = do
  contents <- readFile filepath
  processData $ contents

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