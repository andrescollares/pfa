import Test.QuickCheck

-- a)
esPalindromo :: (Eq a) => [a] -> Bool
esPalindromo l = l == reverse l

-- b)
-- impone alguna restricción, es decír, no chequear con units supongo.



-- c)

implies :: Bool -> Bool -> Bool
implies x y = not x || y

palindromoConcatIsPalindromo :: (Eq a) => [a] -> Bool 
palindromoConcatIsPalindromo l = esPalindromo l `implies` esPalindromo (l ++ l)
