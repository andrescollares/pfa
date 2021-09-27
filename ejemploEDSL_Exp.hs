{-# Language RecordWildCards #-}

import EDSL_Exp

-- ejemplos

a = varD "a"
b = varB "b"
c = varD "c"

e1 :: E Double
e1 = ifE ((a >: c) ||: (c ==: 5)) (a + 3) (c * 2) 

e2 :: E Bool
e2 = ifE (b ||: (a >: 2)) true (c ==: 4) 

-- Estructura que simula una planilla

data Adder = Adder { x :: E Double
                   , y :: E Double
                   , z :: E Double } 
             deriving Show

-- ejemplos

f :: E Double -> E Double -> Adder
f x y = Adder x y (ifE (x >: y) x y) 

g :: Adder -> Adder
g (Adder {..}) = Adder {x=x,y=y,z=x+y}

add1 = f 3 4
add2 = g add1
