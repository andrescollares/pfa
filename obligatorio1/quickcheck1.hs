import Test.QuickCheck

f :: [a] -> [a]
f = foldl (flip (:)) []

prop_f :: Eq a => [a] -> Bool
prop_f xs = f xs == xs

