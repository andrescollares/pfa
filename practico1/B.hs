module B where
import A

data T = undefined

baz :: T -> Int
baz x = foo x x
qux :: Int -> Int
qux x = baz (bar x )
