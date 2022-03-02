{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
type ID = Integer

class Monad c => ControlCOVID c where
  entra :: ID -> c ()
  sale :: ID -> c ()
  listado :: c () -> [ID]
  contactos :: ID -> c () -> [ID]

-- persona -> tiene id
-- entra y sale registran las entradas y saalidas a la institución
-- listado genera un listado de quienes han estado en la institución
-- contactos retorna la lista de contactos que tuvo un caso positivo dentro de la institución

-- Interfaz monádica

registro :: ControlCOVID c => c ()
registro = do
  entra 1
  entra 2
  sale 1
  entra 3
  entra 4
  sale 3
  sale 2
  sale 4

type InOuts = [InOut]

data InOut = In ID | Out ID
  deriving (Eq, Show)

newtype Contactos a = Contactos {runContactos :: InOuts -> (a, InOuts)}
evalContactos c = snd $ runContactos c []

instance Functor Contactos where
    fmap f (Contactos s) = Contactos $ \st -> let (x, st') = s st
                                            in (f x, st')

instance Applicative Contactos where
    pure x = Contactos $ \st -> (x, st)
    c1 <*> c2 = Contactos $ \st -> let (f, st') = runContactos c1 st
                                       (x, st'') = runContactos c2 st'
                                   in (f x, st'')

instance Monad Contactos where
    return = pure
    c >>= f = Contactos $ \st -> let (x, st') = runContactos c st
                                 in runContactos (f x) st'
type STree a = Tree () a

data Tree t a = Zero t | Succ (Tree (Node t a) a)

data Node t a = Node2 t a t | Node3 t a t a t

class Elem t a | t -> a where
    elemT :: Eq a => a -> t -> Bool

instance Elem t a => Elem (Tree t a) a where
    elemT x (Succ t) = elemT x t
    elemT x (Zero t) = elemT x t

instance Elem t a => Elem (Node t a) a where
    elemT x (Node2 t y t') = x == y || elemT x t || elemT x t'
    elemT x (Node3 t y t' y' t'') = x == y || x == y' || elemT x t' || elemT x t''

instance Elem () a where
    elemT _ _ = False
