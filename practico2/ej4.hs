type STree a = Tree () a

data Tree t a = Zero t | Succ (Tree (Node t a) a)

data Node t a = Node2 t a t | Node3 t a t a t

-- a)
a1, a2, a3 :: STree Integer
a1 = Succ (Zero (Node2 () 2 ()))
a2 = Succ (Succ (Zero (Node2 (Node2 () 2 ()) 20 (Node2 () 40 ()))))
a3 = Succ (Succ (Succ (Zero (Node2 (Node2 (Node2 () 1 ()) 10 (Node2 () 15 ())) 20 (Node3 (Node2 () 25 ()) 30 (Node2 () 35 ()) 40 (Node2 () 45 ()))))))

-- b)

instance (Show t, Show a) => Show (Tree t a) where
  show (Zero t) = show t
  show (Succ t) = show t

instance (Show t, Show a) => Show (Node t a) where
  show (Node2 l x r) = "[ " ++ show l ++ " <- " ++ show x ++ " -> " ++ show r ++ " ]"
  show (Node3 l x m y r) = "[ " ++ show l ++ " <- " ++ show x ++ " ( " ++ show m ++ " ) " ++ show y ++ " -> " ++ show r ++ " ]"

-- c)
-- DUDA: Deben aplicarse sobre STree o sobre Tree t a?
-- en caso que sea Tree t a, esta bien forzar que sea Cant t?
-- En el caso de cant, está bien lo escrito aca?
--
-- No se puede implementar dado que necesito acceder a los nodos para saber la cantidad de valores
-- pero no se puede hacer pattern matching en Node sobre t en (Zero t), en resumen, haskell no sabe que luego de un Zero vienen
-- Nodes, lo único que sabe es que es algo del tipo t, pero en las instancias esto cambia dado que podemos indicar que t es una instancia de Cant.

{- cant :: Tree t a -> Int
cant (Zero t) = case t of
    (Node2 l x r) -> 1 + cant' l + cant' r
    (Node3 l x m y r) -> 2 + cant' l + cant' m + cant' r
cant (Succ t) = cant t

cant' :: Node t a -> Int
cant' (Node2 l x r) = 1 + cant' l + cant' r
cant' (Node3 l x m y r) = 2 + cant' l + cant' m + cant' r -}

-- i)

class Cant t where
  cant :: t -> Int

instance (Cant t) => Cant (Tree t a) where
  cant (Zero t) = cant t
  cant (Succ t) = cant t

instance (Cant t) => Cant (Node t a) where
  cant (Node2 l x r) = 1 + cant l + cant r
  cant (Node3 l x m y r) = 1 + cant l + cant m + cant r

instance Cant () where
  cant _ = 0

-- d)
-- Al igual que en el caso anterior, al no existir la clase que indica que existe
-- elemT para el tipo Node, no puedo hacer la transición de Tree a Node.
--
-- Ver si es posible solucionarla igual que en el caso de cant

{- elemT :: (Eq a) => a -> Tree t a -> Bool
elemT x (Succ t) = elemT x t
elemT x (Zero t) = elemT' x t

elemT' :: (Eq a) => a -> Node t a -> Bool
elemT' x (Node2 l y r) = x == y || elemT' x l || elemT' x r
elemT' x (Node3 l y m z r) = x == y || x == z || elemT' x l || elemT' x m || elemT' x r -}
