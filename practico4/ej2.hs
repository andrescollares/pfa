Pair = \f.\s.\b.b f s
Z = \s.\z.z
S = \n.\s.\z.s (n s z)


SWAP : Pair a b -> Pair b a
SWAP = \f.\s.\b.b s f

DUP : Nat -> Nat
DUP = \n.\s.\z.n s (n s z) -- Hay que ver que forma elegir
DUP = \n.\s.n s n  -- En teoría esto debería andar porque a si mismo le estoy sumando el mismo valor

-- mult = λnmf.n(mf). segun el paper 

2 = \s.\z.s (s z)

EXP2  : Nat -> Nat
EXP2 = (\e.\n.\s.\z.(n s z) (s (s z))) EXP2
EXP2 = (\e.\n.\s.\z.(n s z) 2) EXP2

-- e es para abstraer la funcion y poder llamarla a si misma
-- n, s y z son utilizados para obtener el natural ingresado.
-- exp2 :: Int -> Int
-- exp2 0 = 1
-- exp2 n = 2 * exp n -1
-- exp2 = (\n -> 2 * exp n -1)
-- exp2 = (\f n -> if n > 0 then 2 * f (n -1) else 1) exp2