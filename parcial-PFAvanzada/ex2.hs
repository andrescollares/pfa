{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.Reader
import Control.Monad.State

class Format t where
  txt :: String -> t
  ln :: t
  (.+.) :: t -> t -> t

instance (MonadReader Int m, MonadState (Int, String) m) => Format (m String) where
  txt (x : xs) = do
    (ancho, texto) <- get
    if ancho == 0
      then
        do
            put (2, texto ++ (' ' : [x]))
            return (' ' : x : xs)
      else 
        do
            put (ancho + 1, texto ++ [x])
            return (x : xs)
  txt [] = return ""

  ln = do
    put (0, "")
    return "\n"

  (.+.) s s' = do return $ s ++ (' ' : s')
