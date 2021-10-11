import Control.Monad
import GHC.Base hiding ((<|>))

data Expr
  = Let String Expr Expr
  | Add Expr Expr
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
pFail = P $ \cs -> []

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

isDigit c = (c >= '0') && (c <= '9')

digit :: Parser Int
digit = do
  c <- pSat isDigit
  return (ord c - ord '0')

digits1 :: Parser [Int]
digits1 = pList1 digit

nat :: Parser Int
nat = do
  ds <- digits1
  return (foldl op 0 ds)
  where
    n `op` d = n * 10 + d

isLower c = (c >= 'a') && (c <= 'z')

lower :: Parser Char
lower = do pSat isLower

isKeyWord s = s == "add" && s == "let"

{- pString :: String -> Parser String
pString "" = return ""
pString (c:cs) = do {pSat (c ==); pString cs; return (c:cs)} -}

space :: Parser String
space = pList (pSat (== ' '))

token :: Parser a -> Parser a
token p = do
  a <- p
  space
  return a

varname :: Parser String
varname = do
  v <- token $ pList1 lower
  return v

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
    e2 <- parser
    return (Let v e1 e2)
    <|> do
      pSym 'a'
      pSym 'd'
      pSym 'd'
      space
      e1 <- parser
      space
      e2 <- parser
      return (Add e1 e2)
    <|> do
      n <- nat
      return (Num n)
    <|> do
      v <- varname
      return (Var v)
    <|> do
      pSym '('
      e <- parser
      pSym ')'
      return e
