module Processor where

import Stack
import System.IO
import Data.Bits
import Data.Maybe
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

-- Lists all supported operators
supportedOperators :: [String]
supportedOperators = words "+ - * / % < <= > >= == & | ! dup swap peek pop size nil"

supportedSyntax :: [String]
supportedSyntax = words "if: loop:"

-- Applies token to the current state resulting in a different state
evalToken :: [Macro] -> State -> Token -> State
evalToken macros (vars, Stack stack, out) token
  | token == ","                                              = (vars, snd . evalSinglePop $ Stack stack, out ++ (show . fst . evalSinglePop $ Stack stack) ++ [' '])
  | token == "."                                              = (vars, snd . evalSinglePop $ Stack stack, out ++ (show . fst . evalSinglePop $ Stack stack) ++ ['\n'])
  | token !! 0 == '@'                                         = (setVar vars (tail token) $ fst . evalSinglePop $ Stack stack, snd . evalSinglePop $ Stack stack, out)
  | elem token supportedOperators                             = (vars, evalExpression token $ Stack stack, out)
  | validSyntax token supportedSyntax                   = (vars, evalExpression token $ Stack stack, out)
  | elem token $ map (\(x,_) -> x) vars                       = (vars, push (fromJust $ getVar vars token) $ Stack stack, out)
  | otherwise                                                 = (vars, push (read token :: Integer) $ Stack stack, out)

validSyntax _ [] = False
validSyntax token (o:os) = if elem o $ inits token then True else validSyntax token os

evalExpression :: String -> Stack Integer -> Stack Integer
evalExpression token (Stack stack) =
  let evalSingleArg op (Stack stack) = push (op (fst . evalSinglePop $ Stack stack)) (Stack $ drop 1 stack)
      evalDoubleArg op (Stack stack) = push (op (fst . evalPop 2 $ Stack stack) (fst . evalSinglePop $ Stack stack)) (Stack $ drop 2 stack)
      (!) a = if a == 0 then 1 else 0
      (lt) a b = if a < b then 1 else 0
      (le) a b = if a <= b then 1 else 0
      (gt) a b = if a > b then 1 else 0
      (ge) a b = if a >= b then 1 else 0
      (eq) a b = if a == b then 1 else 0

  in case token of
    "+" -> evalDoubleArg (+) $ Stack stack
    "-" -> evalDoubleArg (-) $ Stack stack
    "*" -> evalDoubleArg (*) $ Stack stack
    "/" -> evalDoubleArg (div) $ Stack stack
    "%" -> evalDoubleArg (mod) $ Stack stack
    "<" -> evalDoubleArg (lt) $ Stack stack
    "<=" -> evalDoubleArg (le) $ Stack stack
    ">" -> evalDoubleArg (gt) $ Stack stack
    ">=" -> evalDoubleArg (ge) $ Stack stack
    "==" -> evalDoubleArg (eq) $ Stack stack
    "&" -> evalDoubleArg (.&.) $ Stack stack
    "|" -> evalDoubleArg (.|.) $ Stack stack
    "!" -> evalSingleArg (!) $ Stack stack
    "dup" -> push (fromJust . peek $ Stack stack) $ Stack stack
    "swap" -> push (fst . evalPop 2 $ Stack stack) $ push (fst . evalSinglePop $ Stack stack) (snd . evalPop 2 $ Stack stack)
    "peek" -> push (fromJust . peek $ Stack stack) $ Stack stack
    "pop" -> snd . evalSinglePop $ Stack stack
    "size" -> push (toInteger . size $ Stack stack) $ Stack stack
    "nil" -> Stack stack
    _ | take 3 token == "if:" -> if (!) (fst . evalSinglePop $ Stack stack) == 0 then push (read $ (splitOn ":" token) !! 1) $ Stack stack else push (read $ (splitOn ":" token) !! 2) $ Stack stack
    _ | take 5 token == "loop:" -> Stack stack

splitOn x str = splitOn' x [] "" str
        where
          splitOn' :: String -> [String] -> String -> String -> [String]
          splitOn' _ result substring [] = result ++ [substring]
          splitOn' x result substring (t:ts) = if x == [t] then splitOn' x (result ++ [substring]) "" ts else splitOn' x result (substring ++ [t]) ts

evalPop :: Int -> Stack Integer -> (Integer, Stack Integer)
evalPop n (Stack stack) = (fromJust . fst . pop $ Stack $ drop (n-1) stack, Stack $ drop n stack)

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