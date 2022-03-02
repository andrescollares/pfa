{-# LANGUAGE GADTs #-}

data Empty
data NonEmpty
data SafeList a b where
    Nil :: SafeList a Empty
    Cons :: a -> SafeList a b -> SafeList a NonEmpty

instance Show a => Show (SafeList a b) where
    show (Cons a Nil) = show a
    show (Cons a ls) = show a ++ " " ++ show ls
    show Nil = ""

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

-- d)
data Zero
data Succ a
data Vec a n where
    NilV :: Vec a Zero
    ConsV :: a -> Vec a n -> Vec a (Succ n)

toSafe :: Vec a (Succ n) -> SafeList a NonEmpty 
toSafe (ConsV a (ConsV a' ls)) = Cons a (toSafe (ConsV a' ls))
toSafe (ConsV a NilV) = Cons a Nil
