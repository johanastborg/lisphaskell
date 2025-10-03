--module Main where

--main :: IO ()
--main = putStrLn "Hello, Haskell!"

-- Main.hs
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Data.IORef

-- AST & Error Types
--------------------------------------------------

data LispVal
  = Atom String
  | List [LispVal]
  | Number Integer
  | String String
  | Bool Bool
  | PrimitiveFunc ([LispVal] -> IOThrowsError LispVal)

instance Show LispVal where
  show (Atom name) = name
  show (List contents) = "(" ++ unwords (map show contents) ++ ")"
  show (Number val) = show val
  show (String val) = "\"" ++ val ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (PrimitiveFunc _) = "<primitive>"

data LispError
  = UnboundVar String
  | TypeMismatch String LispVal
  | BadSpecialForm String LispVal
  | NotFunction String
  | NumArgs Integer [LispVal]
  | Default String

instance Show LispError where show = showError

showError :: LispError -> String
showError (UnboundVar msg) = "Unbound variable: " ++ msg
showError (TypeMismatch expected found) = "Type mismatch: expected " ++ expected ++ ", found " ++ show found
showError (BadSpecialForm msg form) = "Bad special form: " ++ msg ++ " -> " ++ show form
showError (NotFunction msg) = "Not a function: " ++ msg
showError (NumArgs expected found) = "Incorrect number of arguments: expected " ++ show expected ++ ", found " ++ show (length found)
showError (Default msg) = msg

type Env = IORef (Map.Map String LispVal)
type IOThrowsError = ExceptT LispError IO

-- Parser
--------------------------------------------------

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escapedChar <|> noneOf "\"")
  char '"'
  return $ String x
  where escapedChar = do { char '\\'; c <- oneOf "\\\"nrt"; return $ case c of {'\\' -> c; '"' -> c; 'n' -> '\n'; 'r' -> '\r'; 't' -> '\t'} }

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces1

parseExpr :: Parser LispVal
parseExpr = try parseNumber <|> parseAtom <|> parseString <|> parseQuoted <|> do { char '('; x <- parseList; char ')'; return x; } <?> "Expression"

spaces1 :: Parser ()
spaces1 = skipMany1 space

-- Evaluator
--------------------------------------------------

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(Number _) = return val
eval env val@(String _) = return val
eval env val@(Bool _) = return val
eval env (Atom var) = do
    envMap <- liftIO $ readIORef env
    case Map.lookup var envMap of
        Just val -> return val
        Nothing  -> throwError $ UnboundVar var
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        _          -> eval env conseq
eval env (List [Atom "define", Atom var, form]) = do
    evalVal <- eval env form
    envMap <- liftIO $ readIORef env
    liftIO $ writeIORef env (Map.insert var evalVal envMap)
    return $ Atom var
eval env (List (func : args)) = do
    functor <- eval env func
    argVals <- mapM (eval env) args
    apply functor argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = func args
apply notFunc _ = throwError $ NotFunction (show notFunc)

-- Primitives & Environment
--------------------------------------------------

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makeFunc primitives)
    where makeFunc (var, func) = (var, PrimitiveFunc func)

nullEnv :: IO Env
nullEnv = newIORef Map.empty

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= \env ->
  extendEnv env bindings >>= newIORef
  where extendEnv env bindings = return $ Map.union (Map.fromList bindings) env

primitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
primitives = [("+", numericBinop (+)), ("-", numericBinop (-)), ("*", numericBinop (*)), ("/", numericBinop div),
              ("mod", numericBinop mod), ("=", numBoolBinop (==)), ("<", numBoolBinop (<)), (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)), (">=", numBoolBinop (>=)), ("<=", numBoolBinop (<=)), ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)), ("string=?", strBoolBinop (==)),
              ("car", car), ("cdr", cdr), ("cons", cons)]

-- Helper functions for primitives
unpackNum :: LispVal -> IOThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> IOThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackStr :: LispVal -> IOThrowsError String
unpackStr (String s) = return s
unpackStr notStr = throwError $ TypeMismatch "string" notStr

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> IOThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = Number . foldl1 op <$> mapM unpackNum params

boolBinop :: (LispVal -> IOThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> IOThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

car :: [LispVal] -> IOThrowsError LispVal
car [List (x : _)] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> IOThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> IOThrowsError LispVal
cons [x, List xs] = return $ List (x : xs)
cons [_, badType] = throwError $ TypeMismatch "list" badType
cons badArgList = throwError $ NumArgs 2 badArgList

-- REPL
--------------------------------------------------

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT action >>= return . either show id

readExpr :: String -> IOThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Default (show err)
    Right val -> return val

main :: IO ()
main = do
  env <- primitiveBindings
  hSetBuffering stdout NoBuffering
  putStrLn "Haskell Lisp Interpreter"
  putStrLn "Enter expressions, or :q to quit."
  forever $ do
    putStr "lis.hs> "
    line <- getLine
    if line == ":q"
      then putStrLn "Goodbye!" >> void (return ())
      else do
        let parsed = readExpr line
        result <- runIOThrows $ parsed >>= eval env >>= return . show
        putStrLn result
