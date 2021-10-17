{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Control.Monad.Catch.Pure (Handler (Handler))
import Control.Monad.Except
import Control.Monad.Reader
-- import Control.Monad.Trans.Reader (runReader)

import Control.Monad.Trans.Reader (ReaderT (ReaderT))
import qualified Data.Functor.Identity as Data
import qualified Data.Functor.Identity as Data.Identity
import GHC.Base hiding (Error, (<|>))
import System.Posix.ByteString (PathVar (FileSizeBits))

data Expr
  = Let String Expr Expr
  | Add Expr Expr
  | Div Expr Expr
  | Num Int
  | Var String
  deriving (Show)

newtype Parser a = P {runP :: String -> [(a, String)]}

instance Functor Parser where
  fmap f p = P $ \cs -> [(f a, cs') | (a, cs') <- runP p cs]

instance Applicative Parser where
  pure a = P (\cs -> [(a, cs)])

  -- (<*>) ::  Parser (a -> b) -> Parser a -> Parser b
  (P p) <*> (P q) = P $ \cs ->
    [ (f a, cs'') | (f, cs') <- p cs, (a, cs'') <- q cs'
    ]

instance Monad Parser where
  return a = P $ \cs -> [(a, cs)]
  (P p) >>= f = P $ \cs -> concat [runP (f a) cs' | (a, cs') <- p cs]

pFail :: Parser a
pFail = P $ const []

item :: Parser Char
item = P $ \cs -> case cs of
  "" -> []
  (c : cs) -> [(c, cs)]

pSat :: (Char -> Bool) -> Parser Char
pSat p = do
  c <- item
  if p c
    then return c
    else pFail

pSym :: Char -> Parser Char
pSym c = pSat (== c)

(<|>) :: Parser a -> Parser a -> Parser a
(P p) <|> (P q) = P $ \cs -> case p cs ++ q cs of
  [] -> []
  (x : xs) -> [x]

pList :: Parser a -> Parser [a]
pList p =
  do
    a <- p
    as <- pList p
    return (a : as)
    <|> return []

pList1 :: Parser a -> Parser [a]
pList1 p = do
  a <- p
  as <- pList p
  return (a : as)

isDigit c = c >= '0' && c <= '9'

digit :: Parser Int
digit = do
  c <- pSat isDigit
  return (ord c - ord '0')

digits1 :: Parser [Int]
digits1 = pList1 digit

nat :: Parser Int
nat = do
  foldl op 0 <$> digits1
  where
    n `op` d = n * 10 + d

isLower c = (c >= 'a') && (c <= 'z')

lower :: Parser Char
lower = pSat isLower

space :: Parser String
space = pList (pSat (== ' '))

token :: Parser a -> Parser a
token p = do
  a <- p
  space
  return a

varname :: Parser String
varname = do
  token $ pList1 lower

parser :: Parser Expr
parser =
  do
    pSym 'l'
    pSym 'e'
    pSym 't'
    space
    v <- varname
    space
    e1 <- parser
    space
    Let v e1 <$> parser
    <|> do
      pSym 'a'
      pSym 'd'
      pSym 'd'
      space
      e1 <- parser
      space
      Add e1 <$> parser
    <|> do
      pSym 'd'
      pSym 'i'
      pSym 'v'
      space
      e1 <- parser
      space
      Div e1 <$> parser
    <|> do
      Num <$> nat
    <|> do
      Var <$> varname
    <|> do
      pSym '('
      e <- parser
      pSym ')'
      return e

data Error = ErrorDivZero | ErrorUnbound String

type Env = [(String, Int)]

addVar :: (String, Int) -> Env -> Env
addVar (v, e) [] = [(v, e)]
addVar (v1, e1) ((v, e) : vars) = if v1 == v then (v1, e1) : vars else (v, e) : addVar (v1, e1) vars

getVarValue :: (MonadReader Env m, MonadError Error m) => Env -> String -> m Int
getVarValue [] v1 = throwError $ ErrorUnbound (show v1 ++ " no está ligada")
getVarValue ((v, e) : vars) v1 = if v == v1 then return e else getVarValue vars v1

getRight :: Either Error Int -> Int
getRight (Right i) = i
getRight _ = 0

getLeft :: Either Error Int -> Error
getLeft (Left e) = e
getLeft _ = undefined

evalE :: Expr -> Either Error Int
evalE e =
  do
    val <- runReaderT (interp e) []
    Right val
    `catchError` handler
  where
    handler ErrorDivZero = Left ErrorDivZero
    handler (ErrorUnbound s) = Left (ErrorUnbound s)

handler :: (Monad m) => Error -> m Int
handler ErrorDivZero = return 0
handler (ErrorUnbound s) = return 0

interp :: (MonadReader Env m, MonadError Error m) => Expr -> m Int
interp (Let v e1 e2) = local (addVar (v, getRight (evalE e1))) $ interp e2
interp (Div e1 e2) = do
  a <- interp e1
  b <- interp e2
  if b == 0 then throwError ErrorDivZero else return (a `div` b)
interp (Add e1 e2) = do
  a <- interp e1
  b <- interp e2
  return (a + b)
interp (Num n) = return n
interp (Var v) = do
  vars <- ask
  getVarValue vars v

instance Show Error where
  show ErrorDivZero = "división por cero"
  show (ErrorUnbound s) = s

main :: IO ()
main = do
  let file = "ej4.txt"
  text <- readFile file
  -- print $ fst $ head $ runP parser text
  print $ evalE $ fst $ head $ runP parser text
  return ()
