import Test.QuickCheck
import Prelude hiding (reverse, (++))

newtype Palindrome a = P [a]
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Palindrome a) where
  arbitrary = sized arbitraryPalindrome

unwrap :: Palindrome a -> [a]
unwrap (P x) = x

-- impone alguna restricción, es decír, no chequear con units supongo.
arbitraryPalindrome :: Arbitrary a => Int -> Gen (Palindrome a)
arbitraryPalindrome 0 = return (P [])
arbitraryPalindrome n =
  frequency
    [ (1, return (P [])),
      ( 4,
        do
          x <- arbitrary
          list <- arbitraryPalindrome (n - 1)
          return (P (x ++ unwrap list ++ reverse x))
      )
    ]

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

neutroIzq :: Eq a => [a] -> Bool
neutroIzq xs = xs ++ [] == xs

neutroDer :: Eq a => [a] -> Bool
neutroDer xs = [] ++ xs == xs

asociativa :: Eq a => [a] -> [a] -> [a] -> Bool
asociativa xs ys zs = xs ++ (ys ++ zs) == (xs ++ ys) ++ zs

reverseConcat :: Eq a => [a] -> [a] -> Bool
reverseConcat xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

implies :: Bool -> Bool -> Bool
implies x y = not x || y

esPalindromo :: (Eq a) => [a] -> Bool
esPalindromo l = l == reverse l

esPalindromo' :: (Eq a) => Palindrome a -> Bool
esPalindromo' (P l) = l == reverse l

prop1 :: Eq a => [a] -> Bool
prop1 xs = implies (esPalindromo xs) (esPalindromo (xs ++ xs))

prop1' :: Eq a => Palindrome a -> Bool
prop1' (P xs) = implies (esPalindromo xs) (esPalindromo (xs ++ xs))