fix f = f (fix f)

-- suma1 f n = if n == 0 then 0 else n + f (n - 1)
-- suma n = suma1 (fix suma1) n
-- Equivale a:
-- suma n = if n == 0 then 0 else n + fix suma1 (n - 1)

suma = fix sum
    where sum f 0 = 0
          sum f n = n + f (n - 1)
