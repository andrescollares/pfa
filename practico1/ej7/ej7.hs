data Arbol a = Vacio | Nodo (Arbol a) a (Arbol a)

genera = fst $ generaAux 0
  where
    generaAux n =
      let (l, n') = generaAux (n + 1)
          (r, n'') = generaAux n'
       in (Nodo l n r, n'')

recorreL (Nodo l x _) = x : recorreL l
recorreL Vacio = []

recorreR (Nodo _ x r) = x : recorreR r
recorreR Vacio = []

recorre (Nodo l x r) = recorre l ++ [x] ++ recorre r
recorre Vacio = []

take' _ [] = []
take' n (x : xs)
  | n > 0 = x : take' (n - 1) xs
  | otherwise = []

main = do
  print (take' 5 . tail . recorreL $ genera)
  print (head $ zip (recorreL genera) (recorreR genera))
