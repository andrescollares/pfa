import Test.QuickCheck

-- a)
esPalindromo :: (Eq a) => [a] -> Bool
esPalindromo l = l == reverse l

-- b)


-- c)

implies :: Bool -> Bool -> Bool
implies x y = not x || y

palindromoConcatIsPalindromo :: (Eq a) => [a] -> Bool
palindromoConcatIsPalindromo l = esPalindromo l `implies` esPalindromo (l ++ l)

iEPC :: [Int] -> Property 
iEPC xs = esPalindromo xs ==> esPalindromo (xs ++ xs)  

-- quickCheckWith stdArgs { maxSuccess = 200, maxDiscardRatio = 100000000000 } iEPC

data Palindrome a = P [a]
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

esPalindromo' :: (Eq a) => Palindrome a -> Bool
esPalindromo' (P l) = l == reverse l

