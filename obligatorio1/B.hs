module B where
import A
-- baz :: A.T -> Int
baz x = foo x x
qux :: Int -> Int
qux x = baz (bar x )
