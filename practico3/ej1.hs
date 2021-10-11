import Control.Monad.State

f = do
  x <- g1
  y <- g2
  return x

h1 = do
  x <- f
  z <- g3
  return (x, z)

h2 = do
  x <- g1
  y <- g2
  z <- g3
  return (x, z)

h3 = do
  x <- g1
  z <- g3
  return (x, z)

g1 :: State Int Int
g1 = do
  state <- put 1
  return get state

g2 :: State Int ()
g2 = modify (+ 1)

g3 :: State Int Int
g3 = get

main = do
  print $ evalState h1 1
  print $ evalState h2 1
  print $ evalState h3 1
