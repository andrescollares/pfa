{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- import Control.Monad
-- import Control.Monad.Catch.Pure (Handler (Handler))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Reader (ReaderT (ReaderT))
import Control.Monad.Identity
import qualified Data.Functor.Identity as Data
import qualified Data.Functor.Identity as Data.Identity
import GHC.Base hiding (Error, (<|>))

data Expr
  = Let String Expr Expr
  | Add Expr Expr
  | Div Expr Expr
  | Num Int
  | Var String
  deriving (Show)

data Error = ErrorDivZero | ErrorUnbound String

type Env = [(String, Int)]

-- a)

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

-- b)

instance Show Error where
  show ErrorDivZero = "división por cero"
  show (ErrorUnbound s) = s

evalE :: Expr -> Either Error Int
evalE e = join $ runExceptT $ runReaderT (runIdentityT (interp e)) []

-- c)

data Res a = DivZero
           | Unbound String
           | Res a

instance Functor Res where
    fmap f (Res a) = Res (f a)
    fmap f (Unbound s) = Unbound s
    fmap f DivZero = DivZero

instance Applicative Res where
    pure a = Res a
    (Res g) <*> (Res k) = Res (g k)
    _ <*> DivZero = DivZero
    _ <*> (Unbound s) = Unbound s
    DivZero <*> _ = DivZero
    (Unbound s)  <*> _ = Unbound s

instance Monad Res where
    return a = Res a
    DivZero >>= _ = DivZero
    (Unbound s) >>= _ = Unbound s
    (Res a) >>= f = f a
    

newtype ResT m a = ResT {runResT :: m (Res a)}

instance Monad m => Functor (ResT m) where
    fmap f (ResT g) = ResT $ do {a <- g; return $ fres f a}
        where fres f (Res a) = Res $ f a
              fres _ DivZero = DivZero
              fres _ (Unbound s) = Unbound s

instance Monad m => Applicative (ResT m) where
    pure a = ResT $ return (Res a)
    (ResT g) <*> (ResT k) = ResT $ do {f <- g; a <- k; return $ fres f a}
        where fres (Res f) (Res a) = Res $ f a
              fres _ DivZero = DivZero
              fres _ (Unbound s) = Unbound s
              fres DivZero _ = DivZero
              fres (Unbound s) _ = Unbound s

instance Monad m => Monad (ResT m) where
    return a = ResT $ return (Res a)
    m >>= f  = ResT $ do {a <- runResT m; runResT (fres f a)}
        where fres f (Res a) = f a
              fres _ DivZero = ResT $ return DivZero
              fres _ (Unbound s) = ResT $ return $ Unbound s

-- d)

instance Monad m => MonadError Error (ResT m) where
  throwError (ErrorUnbound s) = ResT $ return $ Unbound s
  throwError ErrorDivZero = ResT $ return DivZero
  
  catchError (ResT m) _ = ResT m

evalR :: Expr -> Res Int
evalR e = join $ runResT $ runReaderT (runIdentityT (interp e)) []

instance Show (Res Int) where
    show (Res a) = show a
    show (Unbound s) = s
    show DivZero = "Division por cero"






