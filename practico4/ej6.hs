{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

type STree a = Tree () a

data Tree t a = Zero t | Succ (Tree (Node t a) a)

data Node t a = Node2 t a t | Node3 t a t a t

-- Definir las instancias de:

class Elem t a where
    elemT :: a -> t -> Bool

instance Elem (Tree t a) a where
    elemT e (Zero t) = undefined
    elemT e (Succ t) = undefined

instance Elem (Node t a) a where
    elemT 
