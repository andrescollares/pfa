-- import Data.List (foldl')

-- modificar esta variable para la ejecución, comentar el código en base a la parte a ejecutar
n = 1000000

-- (a)
main = print $ f [1 .. n]

-- i.
-- f = foldl (flip (:)) [ ]
-- i'.
-- f = foldl' (flip (:)) []
-- ii.
f = foldr (\x r -> r ++ [x]) []
 
-- (b)
-- main = print $ f [1 .. n] [1 .. n]

-- i.
-- f = \xs ys -> foldr (:) ys xs
-- ii.
-- f = foldl (\k x -> k . (x :)) id

-- (c)
-- main = print f

-- i.
-- f = let xs = [1 .. n ] in if length xs > 0 then head xs else 0
-- ii.
-- f = if length [1 .. n] > 0 then head [1 .. n] else 0
