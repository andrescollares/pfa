-- Andrés Collares (4.867.464-5)

{-# LANGUAGE ExistentialQuantification,
             GADTs,
             KindSignatures
#-}

-- | Arboles binarios con info en los nodos + info de estructura
data Tree :: * -> * -> * where
    Empty :: Tree a ()
    Node  :: Tree a l -> a -> Tree a r -> Tree a (l,r)


-- | Definir un arbol de este tipo
tree :: Tree Char (((),((),())),())
tree = Node (Node Empty '2' (Node Empty '3' Empty)) '1' Empty

-- | Referencias dentro de un arbol
data Ref a = RHere | RLeft (Ref a) | RRight (Ref a)

-- | Retorna el valor alojado en el nodo indicado por la referencia
ulookup :: Ref a -> Tree a t -> Maybe a
ulookup RHere (Node _ x _) = Just x
ulookup (RLeft next_ref) (Node l _ _) = ulookup next_ref l 
ulookup (RRight next_ref) (Node _ _ r) = ulookup next_ref r 
ulookup _ Empty = Nothing

 -- | Referencias seguras
data SRef :: * -> * -> * where
  SHere  :: SRef a (l,r)
  SLeft  :: SRef a l -> SRef a (l,r)
  SRight :: SRef a r -> SRef a (l,r)

-- | Retorna el valor alojado en el nodo indicado por la referencia segura
slookup :: SRef a t -> Tree a t -> a
slookup SHere (Node _ x _) = x
slookup (SLeft next_ref) (Node l _ _) = slookup next_ref l
slookup (SRight next_ref) (Node _ _ r) = slookup next_ref r

-- | Par conteniendo un arbol y una referencia segura a un elemento 
data Somewhere a = forall t . S (SRef a t, Tree a t)

-- | Retorna el elemento que indica la referencia
here :: Somewhere a -> a
here (S (ref, t)) = slookup ref t
