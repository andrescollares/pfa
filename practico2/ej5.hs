module Ej5 (SHTML) where

import Data.List (foldl')


class DSL_HTML t where
  text :: String -> t
  bold :: t -> t
  italics :: t -> t
  underline :: t -> t
  url :: String -> t -> t
  size :: Int -> t -> t
  list :: [t] -> t
  (<->) :: t -> t -> t -- Separa usando un salto de linea
  (<+>) :: t -> t -> t -- Separa usando un espacio en blanco
  generate :: t -> String

-- shallow embedded

newtype SHTML = H String
    deriving(Show)

instance DSL_HTML SHTML where
  text t = H t
  bold (H t) = H ("<b>" ++ t ++ "</b>")
  italics (H t) = H ("<i>" ++ t ++ "</i>")
  underline (H t) = H ("<ins>" ++ t ++ "</ins>")
  url url (H t) = H ("<a href='" ++ url ++ "'>" ++ t ++ "</a>")
  size s (H t) = H ("<span style='font-size:'" ++ show s ++ "px'>" ++ t ++ "</span>")
  list ts = H ("<ul>" ++ concatMap (\(H t) -> "<li>" ++ t ++ "</li>") ts ++ "</ul>")
  (<->) (H t1) (H t2) = H (t1 ++ "<br>" ++ t2)
  (<+>) (H t1) (H t2) = H (t1 ++ " " ++ t2)
  generate (H t) = t
  -- generate (H t) = "<html>\n  <head></head>\n  <body>" ++ t ++ "\n  </body>\n</html>"


ex1 :: SHTML
ex1 = text "hola soy un texto sin formato"
ex2 :: SHTML
ex2 = (bold . text) "hola soy bold" <+> (underline . bold . text) "y subrayado"
ex3 :: SHTML
ex3 = ex1 <-> size 35 (ex2 <+> text "y grande") <->
      text "consultar en:" <+>
      url "http://www.google.com"((bold . text) "Google")
ex4 :: SHTML
ex4 = text "la lista es:" <+>
      list [ex1, ex2, (italics . text) "y nada mas"]

main = do writeFile "ex1.html" (generate ex1)
          writeFile "ex2.html" (generate ex2)
          writeFile "ex3.html" (generate ex3)
          writeFile "ex4.html" (generate ex4)

