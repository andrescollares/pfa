{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

type STree a = Tree () a

data Tree t a = Zero t | Succ (Tree (Node t a) a)

data Node t a = Node2 t a t | Node3 t a t a t

-- Definir las instancias de:

class Elem t a where
  elemT :: a -> t -> Bool

instance (Eq a) => Elem (STree a) a where
  elemT e (Zero ()) = False
  elemT e (Succ t) = elemT e t

instance Elem () a where
  elemT e _ = False

instance (Elem t a, Eq a) => Elem (Tree (Node t a) a) a where
  elemT e (Zero (Node2 t1 a t2)) = e == a || elemT e t1 || elemT e t2
  elemT e (Zero (Node3 t1 a t2 b t3)) = e == a || e == b || elemT e t1 || elemT e t2 || elemT e t3
  elemT e (Succ t) = elemT e t

instance (Elem t a, Eq a) => Elem (Node t a) a where
  elemT e (Node2 t1 a t2) = e == a || elemT e t1 || elemT e t2
  elemT e (Node3 t1 a t2 b t3) = e == a || e == b || elemT e t1 || elemT e t2 || elemT e t3