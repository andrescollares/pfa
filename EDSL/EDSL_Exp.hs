{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module EDSL_Exp ( -- E se exporta como un tipo abstracto 
                  E () 
                  -- Se exportan las operaciones sobre E
                  -- además de la instancia de la clase Num
                , varB
                , varD
                , varS
                , true
                , false
                , (&&:)
                , (||:)
                , (==:)
                , (>:)
                , ifE ) where

-- AST para Excel

data Exp = LitDbl Double
         | LitStr String
         | LitBool Bool
         | Apply Func [Exp]
         | Var Id
     deriving Show

type Func = String
type Id = String

-- Phantom type para expresiones

data E a = E Exp

instance Show a => Show (E a) where
  show (E x) = show x

instance Num (E Double) where
  E x + E y     = E (Apply "+" [x, y])
  E x - E y     = E (Apply "-" [x, y])
  E x * E y     = E (Apply "*" [x, y])
  abs    (E x)  = E (Apply "abs" [x])
  signum (E x)  = E (Apply "signum" [x])
  fromInteger x = E (LitDbl (fromInteger x))

-- smart constructors para declaración de variables

var :: Id -> E a
var x = E $ Var x

varB :: Id -> E Bool 
varB = var

varD :: Id -> E Double
varD = var

varS :: Id -> E String
varS = var

-- Smart constructors para operadores booleanos

true :: E Bool
true = E (LitBool True)

false :: E Bool
false = E (LitBool False)

(&&:) :: E Bool -> E Bool -> E Bool
(E x) &&: (E y) = E $ Apply "AND" [x,y]

(||:) :: E Bool -> E Bool -> E Bool
(E x) ||: (E y) = E $ Apply "OR" [x,y]

-- operadores de comparación

(==:) :: E a -> E a -> E Bool
(E x) ==: (E y) = E $ Apply "EQ" [x,y]

(>:) :: E a -> E a -> E Bool
(E x) >: (E y) = E $ Apply "GT" [x,y]

ifE :: E Bool -> E a -> E a -> E a
ifE (E b) (E t) (E e) = E $ Apply "IF" [b,t,e]

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
