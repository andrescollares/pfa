{-# LANGUAGE GADTs #-}

data Empty
data NonEmpty
data SafeList a b where
    Nil :: SafeList a Empty
    Cons :: a -> SafeList a b -> SafeList a NonEmpty

-- a)
safeHead :: SafeList a NonEmpty -> a
safeHead (Cons a ls) = a

-- b)
safeTail :: SafeList a NonEmpty -> Either (SafeList a Empty) (SafeList a NonEmpty)
safeTail (Cons a (Cons a' ls)) = Right (Cons a' ls)
safeTail (Cons a Nil) = Left Nil

-- c)
safeFoldr1 :: (a -> a -> a) -> SafeList a NonEmpty -> a
safeFoldr1 f (Cons a (Cons a' Nil)) = a `f` a'
safeFoldr1 f (Cons a (Cons a' ls)) = a `f` safeFoldr1 f (Cons a' ls)
safeFoldr1 _ (Cons a Nil) = a
