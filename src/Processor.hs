module Processor where

import Stack
import Data.Bits
import Control.Exception

-- Types
type Name = String
type Arity = Int
type Token = String
type Macro = (Name, Arity, [Token])

type Value = Integer
type Variable = (Name, Value)

type State = ([Variable], Stack Integer, String)
type Program = ([Macro], State, [Token])

-- List of all supported operators:
-- , . + - * / % < <= > >= == & | ! dup swap peek pop size nil

-- List of all supported syntax
-- if: loop:

-- Applies token to the current state resulting in a different state
evalToken :: [Macro] -> State -> Token -> State
evalToken macros (vars, Stack stack, out) token =
  let evalSingleArg op (Stack stack) = push (op (fst . evalSinglePop $ Stack stack)) (Stack $ drop 1 stack)
      evalDoubleArg op (Stack stack) = push (op (fst . evalPop 2 $ Stack stack) (fst . evalSinglePop $ Stack stack)) (Stack $ drop 2 stack)
      (!) a = if a == 0 then 1 else 0
      (lt) a b = if a < b then 1 else 0
      (le) a b = if a <= b then 1 else 0
      (gt) a b = if a > b then 1 else 0
      (ge) a b = if a >= b then 1 else 0
      (eq) a b = if a == b then 1 else 0
      loop (vars, _, out) (n, Stack stack) op = loop' (vars, Stack stack, out) op n n
      loop' (vars, Stack stack, out) _ n 0 = (vars, Stack stack, out)
      loop' (vars, Stack stack, out) op n1 n2 = loop' (evalToken macros (vars, (push (n1-(n2-1)) $ Stack stack), out) op) op n1 (n2-1)
      splitOn x str = splitOn' x [] "" str
      splitOn' _ result substring [] = result ++ [substring]
      splitOn' x result substring (t:ts) = if x == [t] then splitOn' x (result ++ [substring]) "" ts else splitOn' x result (substring ++ [t]) ts
      evalPush x (Stack stack) = case x of
        Nothing -> throw (ErrorCall "Peek on empty stack")
        Just x -> push x $ Stack stack
      evalPop n (Stack stack) = case fst . pop $ Stack $ drop (n-1) stack of
        Nothing -> error "Pop on empty stack"
        Just x -> (x, snd . pop $ Stack $ drop (n-1) stack)
      evalSinglePop (Stack stack) = evalPop 1 $ Stack stack
      evalMacro 0 (vars, Stack stack, out) = (vars, Stack stack, out)
      evalMacro n (vars, Stack stack, out) = evalMacro (n-1) (setVar vars (['_'] ++ show n) (fst . evalSinglePop $ Stack stack), snd . evalSinglePop $ Stack stack, out)
      count _ [] = 0
      count e (t:ts) = if e == t then 1 + count e ts else count e ts
      evalConditional macros (vars, (top, Stack stack), out) token
        | count ':' token == 1 = if (!) top == 0 then evalToken macros (vars, Stack stack, out) $ (splitOn ":" token) !! 1 else (vars, Stack stack, out)
        | otherwise            = if (!) top == 0 then evalToken macros (vars, Stack stack, out) $ (splitOn ":" token) !! 1 else evalToken macros (vars, Stack stack, out) $ (splitOn ":" token) !! 2
  in case token of
    "," -> (vars, snd . evalSinglePop $ Stack stack, out ++ (show . fst . evalSinglePop $ Stack stack) ++ [' '])
    "." -> (vars, snd . evalSinglePop $ Stack stack, out ++ (show . fst . evalSinglePop $ Stack stack) ++ ['\n'])
    "+" -> (vars, evalDoubleArg (+) $ Stack stack, out)
    "-" -> (vars, evalDoubleArg (-) $ Stack stack, out)
    "*" -> (vars, evalDoubleArg (*) $ Stack stack, out)
    "/" -> (vars, evalDoubleArg (div) $ Stack stack, out)
    "%" -> (vars, evalDoubleArg (mod) $ Stack stack, out)
    "<" -> (vars, evalDoubleArg (lt) $ Stack stack, out)
    "<=" -> (vars, evalDoubleArg (le) $ Stack stack, out)
    ">" -> (vars, evalDoubleArg (gt) $ Stack stack, out)
    ">=" -> (vars, evalDoubleArg (ge) $ Stack stack, out)
    "==" -> (vars, evalDoubleArg (eq) $ Stack stack, out)
    "&" -> (vars, evalDoubleArg (.&.) $ Stack stack, out)
    "|" -> (vars, evalDoubleArg (.|.) $ Stack stack, out)
    "!" -> (vars, evalSingleArg (!) $ Stack stack, out)
    "dup" -> (vars, evalPush (peek $ Stack stack) $ Stack stack, out)
    "swap" -> (vars, push (fst . evalPop 2 $ Stack stack) $ push (fst . evalSinglePop $ Stack stack) (snd . evalPop 2 $ Stack stack), out)
    "peek" -> (vars, evalPush (peek $ Stack stack) $ Stack stack, out)
    "pop" -> (vars, snd . evalSinglePop $ Stack stack, out)
    "size" -> (vars, push (toInteger . size $ Stack stack) $ Stack stack, out)
    "nil" -> (vars, Stack stack, out)
    _ | token !! 0 == '@' -> (setVar vars (tail token) $ fst . evalSinglePop $ Stack stack, snd . evalSinglePop $ Stack stack, out)
    _ | elem token $ map (\(x,_) -> x) vars -> (vars, push (getVar vars token) $ Stack stack, out)
    _ | token !! 0 == '_' -> (vars, push (getVar vars token) $ Stack stack, out)
    _ | token !! 0 == '$' -> eval (macros, evalMacro (getMacroArity macros $ tail token) (vars, Stack stack, out), getMacroTokens macros $ tail token)
    _ | take 3 token == "if:" -> evalConditional macros (vars, (evalSinglePop $ Stack stack), out) token
    _ | take 5 token == "loop:" -> loop (vars, Stack stack, out) (evalSinglePop $ Stack stack) $ splitOn ":" token !! 1
    _ -> (vars, push (read token :: Integer) $ Stack stack, out)

-- Computes the final state of a program from a starting point state
eval :: Program -> State
eval (_, state, []) = state
eval (macros, state, (token:tokens)) = eval (macros, evalToken macros state token, tokens)

-- Builds up program based on file data and returns result from eval
processData :: String -> State
processData rawFileData = parseResult $ eval (macros, initialState, tokens)
  where
    tokens = words $ unlines $ drop (numMacros + 1) fileData
    initialState = ([], Stack [], "")
    macros = loadMacros numMacros (tail fileData)
    numMacros = read $ head fileData
    loadMacros n (l:ls) = if n == 0 then [] else readMacro l : loadMacros (n-1) ls
    fileData = removeComments $ lines $ rawFileData
    removeComments fd = filter (\line -> head line /= '#') fd
    parseResult (macros, state, out) = if length out > 0 && reverse out !! 0 == '\n' then (macros, state, init out) else (macros, state, out)

-- Reads data from text file and outputs result from processData
run :: FilePath -> IO ()
run filepath = do
  contents <- readFile filepath
  (_, _, output) <- return $ processData contents
  putStrLn output

runCommand :: String -> IO String
runCommand content = do
  state <- try . evaluate . extractStack $ finalState
  case state of
    Left (SomeException ex) -> throw ex
    Right stack -> return . extractOutput $ finalState
  where
    extractStack (_, stack, _) = stack
    extractOutput (_, _, output) = output
    finalState = processData content

-- Reading a macro from string
readMacro :: String -> Macro
readMacro line = readMacro' $ words line
  where
    readMacro' ls = (head ls, (read $ head $ tail ls), (drop 2 ls))

-- Get macro token by name
getMacroTokens :: [Macro] -> Name ->  [Token]
getMacroTokens [] _ = error "No macro found"
getMacroTokens ((name, _, tokens):macros) lookup_name = if name == lookup_name then tokens else getMacroTokens macros lookup_name

-- Get macro arity by name
getMacroArity :: [Macro] -> Name -> Arity
getMacroArity [] _ = error "No macro found"
getMacroArity ((name, arity, _):macros) lookup_name = if name == lookup_name then arity else getMacroArity macros lookup_name

-- Get variable value by name
getVar :: [Variable] -> Name -> Value
getVar [] _ = error "No variable found"
getVar ((name, value):variables) lookup_name = if name == lookup_name then value else getVar variables lookup_name

-- Set variable value by name
setVar :: [Variable] -> Name -> Value -> [Variable]
setVar [] name value = [(name, value)]
setVar ((name, value):variables) lookup_name replace_value
  | name == lookup_name = (name, replace_value):variables
  | otherwise           = (name, value) : setVar variables lookup_name replace_value