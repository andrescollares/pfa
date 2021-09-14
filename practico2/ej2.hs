data Tree a = Leaf a | Bin (Tree a) (Tree a)

instance (Show a) => Show (Tree a) where
  show tree = case tree of
    Leaf x -> show x
    Bin left right -> show left ++ " " ++ show right

-- a)

unfoldTree :: (b -> Either a (b, b)) -> b -> Tree a
unfoldTree next s = case next s of
  Left a -> Leaf a
  Right (b, b') -> Bin (unfoldTree next b) (unfoldTree next b')

-- b)

-- replicateTree n x retorna un árbol balanceado con n hojas cuyo valor es x.
replicateTree :: Int -> b -> Tree b
replicateTree n x = unfoldTree (\n -> if n == 1 then Left x else Right (n `div` 2, n `div` 2)) n

-- fromToTree b e retorna un árbol balanceado tal que, si sus hojas son visitadas
-- de izquierda a derecha se obtiene la lista [b..e].
fromToTree :: Int -> Int -> Tree Int
fromToTree b e =
  unfoldTree
    ( \(n, x) ->
        if n < 1 
          then Left x
          else Right ((n `div` 2, x), (n `div` 2, x + (n `div` 2) + 1))
    )
    (e - b, b)

-- cuando n se vuelve 0 entonces se generaron todos los nodos necesarios.
