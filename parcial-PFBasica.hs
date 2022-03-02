module Main where

main :: IO ()
main = do
    let result = if (eqShape tree tree) then sumTree tree else sumTree_f tree
    print result

-- El siguiente tipo representa a un árbol general donde
-- cada nodo contiene un valor y una lista de hijos
data Tree a = Node a [Tree a]
  deriving (Show, Eq)

-- Defina un valor |tree :: Tree Int| que represente al siguiente árbol:
--          10
--       /   |   \
--      6    9    15
--     / \      /  |  \
--    1   8    12  14  900

tree = Node 10 [(Node 6 [(Node 1 []), (Node 8 [])]), (Node 9 []), (Node 15 [(Node 12 []), (Node 14 []), (Node 900 [])])]

-- Defina una función |eqShape| que dados dos árboles
-- determina si tienen la misma forma (independiente de los
-- valores contenidos)

eqShape :: Tree a -> Tree b -> Bool
eqShape (Node _ (hx : hxs)) (Node _ (hy : hys)) = length (hx : hxs) == length (hy : hys) && and (zipWith eqShape (hx : hxs) (hy : hys))
eqShape (Node _ []) (Node _ []) = True
eqShape _ _ = False

-- Implemente una función |sumTree|, que dado un |Tree a| con a numérico
-- sume los valores de los nodos. Por ejemplo |sumTree tree| da 975.

sumTree :: Num a => Tree a -> a
sumTree (Node x hxs) = x + sum (map sumTree hxs)

-- Escriba una función |depths| que sustituye cada valor de un árbol
-- por su profundidad (La profundidad de la raíz es 0).

depths :: Tree a -> Tree Int
depths = depthsAux 0
  where
    depthsAux d (Node x ts) = Node d (map (depthsAux (d + 1)) ts)

-- Dada la siguiente clase para recorridas sobre árboles:
class TreeTraversals t where
  --  lista los valores contenidos en un árbol |t|.
  flatten :: t a -> [a]

  -- cambia cada elemento de tipo |a| por uno de tipo |b|
  mapTree :: (a -> b) -> t a -> t b

-- Implemente la instancia para |Tree|.
-- flatten debe recorrer el árbol en el siguiente orden:
--  - raíz - hijos
--  - ejemplo: flatten tree = [10,6,1,8,9,15,12,14,900]

instance TreeTraversals Tree where
  flatten (Node x ts) = x : concat (map flatten ts)
  mapTree f (Node x ts) = Node (f x) (map (mapTree f) ts)

-- Defina una función |eqTree| que dados dos árboles |t a|
-- retorna True si contienen los mismos valores en el
-- mismo orden, a pesar de que los árboles puedan tener
-- formas distintas.

eqTree :: (Eq a, TreeTraversals t) => t a -> t a -> Bool
eqTree a b = flatten a == flatten b

-- Defina el fold para |Tree a|, con el siguiente tipo:
-- foldTree :: (a -> [b] -> b) -> Tree a -> b
-- tal que |foldTree f| aplicado a un árbol |t|
-- computa un valor de tipo b usando las función f.

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (Node x ts) = x `f` map (foldTree f) ts

-- Defina |sumTree_f|, que debe ser equivalente a |sumTree| pero
-- implementada en función de |foldTree|

sumTree_f = foldTree (\x xs -> x + sum xs)
