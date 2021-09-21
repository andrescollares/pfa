{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

-- unfoldr
unfoldr :: (s -> Maybe (a, s)) -> s -> [a]
unfoldr next s = case next s of
  Nothing -> []
  Just (a, s') -> a : unfoldr next s'

takeWhileC :: (a -> Bool) -> [a] -> [a]
takeWhileC p = unfoldr gTake
  where
    gTake [] = Nothing
    gTake (x : xs) = if p x then Just (x, xs) else Nothing

zipWithC :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithC f xs ys = unfoldr gZip (xs, ys)
  where
    gZip ([], _) = Nothing
    gZip (_, []) = Nothing
    gZip (x : xs, y : ys) = Just (f x y, (xs, ys))

tailsC :: [a] -> [[a]]
tailsC xs = unfoldr gTails xs ++ [[]]
  where
    gTails [] = Nothing
    gTails (x : xs) = Just (x : xs, xs)

evensC :: [a] -> [a]
evensC = unfoldr gEvens
  where
    gEvens [] = Nothing
    gEvens [x] = Nothing
    gEvens (x : y : xs) = Just (y, xs)

-- Repr
data Repr a = forall s. Repr (s -> Maybe (a, s)) s

toRepr :: [a] -> Repr a
toRepr xs = Repr next xs
  where
    next [] = Nothing
    next (x : xs) = Just (x, xs)

fromRepr :: Repr a -> [a]
fromRepr (Repr next s) = unfoldr next s

takeWhileR :: (a -> Bool) -> [a] -> [a]
takeWhileR p = fromRepr . takeWhileRepr p . toRepr

takeWhileRepr :: (a -> Bool) -> Repr a -> Repr a
takeWhileRepr p (Repr next s) = Repr next' s
  where
    next' s = case next s of
      Nothing -> Nothing
      Just (a, s') -> if p a then Just (a, s') else Nothing

zipWithR :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithR f xs ys = fromRepr . zipWithRepr f $ (toRepr xs, toRepr ys)

zipWithRepr :: (a -> b -> c) -> (Repr a, Repr b) -> Repr c
zipWithRepr f (Repr next x, Repr next' y) = Repr next'' (x, y)
  where
    next'' (x, y) = case next x of
      Nothing -> Nothing
      Just (a, x') -> case next' y of
        Nothing -> Nothing
        Just (b, y') -> Just (f a b, (x', y'))

tailsR :: [a] -> [[a]]
tailsR xs = (fromRepr . tailsRepr $ toRepr xs) ++ [[]]

tailsRepr :: Repr a -> Repr [a]
tailsRepr (Repr next s) = Repr next' s
  where
    next' s = case next s of
      Nothing -> Nothing
      Just (a, s') -> Just (fromRepr $ Repr next s, s')

evensR :: [a] -> [a]
evensR = fromRepr . evensRepr . toRepr

evensRepr :: Repr a -> Repr a
evensRepr (Repr next s) = Repr next' s
  where
    next' s = case next s of
      Nothing -> Nothing
      Just (a, s') -> next'' s
        where
          next'' s = case next s' of
            Nothing -> Nothing
            Just (a', s'') -> Just (a', s'')

-- Stream
data Stream a = forall s. Stream (s -> Step a s) s

data Step a s
  = Done
  | Yield a s
  | Skip s

stream :: [a] -> Stream a
stream xs = Stream next xs
  where
    next [] = Done
    next (x : xs) = Yield x xs

unstream :: Stream a -> [a]
unstream (Stream next s) = unfoldS next s

unfoldS :: (s -> Step a s) -> s -> [a]
unfoldS next s = unfold s
  where
    unfold s = case next s of
      Done -> []
      Yield a s' -> a : unfold s'
      Skip s' -> unfold s'

takeWhileS :: (a -> Bool) -> [a] -> [a]
takeWhileS p = unstream . takeWhileStream p . stream

takeWhileStream :: (a -> Bool) -> Stream a -> Stream a
takeWhileStream p (Stream next s) = Stream next' s
  where
    next' s = case next s of
      Done -> Done
      Yield a s' -> if p a then Yield a s' else Skip s'
      Skip s' -> Skip s'

zipWithS :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithS f xs ys = unstream . zipWithStream f $ (stream xs, stream ys)

zipWithStream :: (a -> b -> c) -> (Stream a, Stream b) -> Stream c
zipWithStream f (Stream next x, Stream next' y) = Stream next'' (x, y)
  where
    next'' (x, y) = case next x of
      Done -> Done
      Skip x' -> Skip (x', y)
      Yield a x' -> case next' y of
        Done -> Done
        Skip y' -> Skip (x, y')
        Yield b y' -> Yield (f a b) (x', y')

-- Skip es la transicion silenciosa, por lo tanto, si tengo skip en uno de los Streams
-- no quiero perder la información que tiene el otro, lo que lleva a que avance solo
-- uno de los dos.
-- No tengo tan claro si eso funciona así.

tailsS :: [a] -> [[a]]
tailsS xs = (unstream . tailsStream $ stream xs) ++ [[]]

tailsStream :: Stream a -> Stream [a]
tailsStream (Stream next s) = Stream next' s
  where
    next' s = case next s of
      Done -> Done
      Skip s' -> Skip s'
      Yield a s' -> Yield (unstream $ Stream next s) s'

evensS :: [a] -> [a]
evensS = unstream . evensStream . stream

evensStream :: Stream a -> Stream a
evensStream (Stream next s) = Stream next' s
  where
    next' s = case next s of
      Done -> Done
      Skip s' -> Skip s'
      Yield a s' -> next'' s
        where
          next'' s = case next s' of
            Done -> Done
            Skip s'' -> Skip s'' -- Esto genera un bug en el caso que ocurra
            Yield a' s'' -> Yield a' s''
