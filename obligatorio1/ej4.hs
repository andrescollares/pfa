import Control.Monad
import Control.Monad.Trans.Class (MonadTrans (lift))
import Test.QuickCheck

data Tree a = Empty | Node (Tree a) a (Tree a)
  deriving (Show)

size :: Tree a -> Int
size Empty = 0
size (Node l _ r) = size l + 1 + size r

flatten :: Tree a -> [a]
flatten Empty = []
flatten (Node l a r) = flatten l ++ [a] ++ flatten r

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized arbitraryTree

arbitraryTree :: Arbitrary a => Int -> Gen (Tree a)
arbitraryTree 0 = return Empty
arbitraryTree n =
  frequency
    [ (1, return Empty),
      (4, liftM3 Node (arbitraryTree (n `div` 2)) arbitrary (arbitraryTree (n `div` 2)))
    ]

mirror :: Tree a -> Tree a
mirror Empty = Empty
mirror (Node l a r) = Node (mirror r) a (mirror l)

prop_flat :: Eq a => Tree a -> Bool
prop_flat tree = flatten (mirror tree) == reverse (flatten tree)

instance Eq a => Eq (Tree a) where
    (==) Empty Empty = True
    (==) (Node l1 x r1) (Node l2 y r2)
        | x == y = l1 == l2 && r1 == r2 
        | otherwise = False
    (==) _ _ = False

prop_inv :: Eq a => Tree a -> Bool
prop_inv tree = tree == mirror (mirror tree)

prop_size :: Tree a -> Bool
prop_size tree = size (mirror tree) == size tree
