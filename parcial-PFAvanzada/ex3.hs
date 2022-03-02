{-# LANGUAGE GADTs #-}

import Data.Map

type Name = String

data Lam t where
  Var :: Name -> Lam Int
  Abs :: Name -> Lam t -> Lam (Int -> t)
  App :: Lam (Int -> t) -> Lam Int -> Lam Int

rightmost :: Lam Int -> Name
rightmost (Var x) = x
rightmost (App exp var) = rightmost var

data Fresh a = Fresh {runFresh :: Map Name Int -> (a, Map Name Int)}

instance Functor Fresh where
  fmap f (Fresh s) = Fresh $ \st ->
    let (a, map) = s st
     in (f a, map)

instance Applicative Fresh where
  pure x = Fresh $ \mp -> (x, mp)
  f1 <*> f2 = Fresh $ \mp ->
    let (f, mp') = runFresh f1 mp
        (x, mp'') = runFresh f2 mp'
     in (f x, mp'')

instance Monad Fresh where
  return = pure
  (>>=) (Fresh r) f = Fresh $ \st ->
    let (a, st') = r st
        Fresh r' = f a
     in r' st'

evalFresh :: Fresh t -> t
evalFresh = fst . flip runFresh empty

fresh :: Name -> Fresh Name
fresh nm = Fresh $ \st -> let n = maybe 0 id (Data.Map.lookup nm st)
                          in  (nm ++ show n, insert nm (n+1) st)

fv :: Lam t -> [Name]
fv (Var x) = [evalFresh $ fresh x]
fv (Abs x lam) = fv lam
fv (App lam x) = fv lam ++ fv x
