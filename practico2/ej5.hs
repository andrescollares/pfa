{-# LANGUAGE GADTs #-}

module Ej5 (SHTML) where

import Data.List (foldl')

class DSL_HTML t where
  text :: String -> t
  bold :: t -> t
  italics :: t -> t
  underline :: t -> t
  url :: String -> t -> t
  size :: Int -> t -> t -- revisar este, el HTML mantiene un campo largo o como?
  list :: [t] -> t -- Utilizar el tipo lista de HTML
  (<->) :: t -> t -> t -- Separa usando un salto de linea
  (<+>) :: t -> t -> t -- Separa usando un espacio en blanco
  generate :: t -> String
  countWords :: t -> Int
  color :: Int -> Int -> Int -> t -> t

-- shallow embedded

newtype SHTML = H String
  deriving (Show)

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
ex3 =
  ex1 <-> size 35 (ex2 <+> text "y grande")
    <-> text "consultar en:"
    <+> url "http://www.google.com" ((bold . text) "Google")

ex4 :: SHTML
ex4 =
  text "la lista es:"
    <+> list [ex1, ex2, (italics . text) "y nada mas"]

-- deep embedded

data HTML where
  Text :: String -> HTML
  Bold :: HTML -> HTML
  Italics :: HTML -> HTML
  Underline :: HTML -> HTML
  Url :: HTML -> HTML -> HTML
  List :: [HTML] -> HTML
  Size :: Int -> HTML -> HTML
  Concat :: HTML -> HTML -> HTML
  ConcatSpace :: HTML -> HTML -> HTML
  Color :: Int -> Int -> Int -> HTML -> HTML

instance DSL_HTML HTML where
  text s = Text s
  bold html = Bold html
  italics html = Italics html
  underline html = Underline html
  url string html = Url (text string) html
  size tam html = Size tam html
  list htmls = List htmls
  color c1 c2 c3 html = Color c1 c2 c3 html
  (<->) html1 html2 = Concat html1 html2
  (<+>) html1 html2 = ConcatSpace html1 html2
  generate = generateDeep
    where
      generateDeep (Text s) = s
      generateDeep (Bold html) = "<b>" ++ generateDeep html ++ "</b>"
      generateDeep (Italics html) = "<i>" ++ generateDeep html ++ "</i>"
      generateDeep (Underline html) = "<ins>" ++ generateDeep html ++ "</ins>"
      generateDeep (Url text html) = "<a href=\"" ++ generateDeep text ++ "\">" ++ generateDeep html ++ "</a>"
      generateDeep (Size size html) = "<span style=\"font-size:" ++ show size ++ "\">" ++ generateDeep html ++ "</span>"
      generateDeep (List htmls) = "<ul>" ++ concat ["<li>" ++ generateDeep html ++ "</li>" | html <- htmls] ++ "</ul>"
      generateDeep (Concat html1 html2) = generateDeep html1 ++ "<br/>" ++ generateDeep html2
      generateDeep (ConcatSpace html1 html2) = generateDeep html1 ++ " " ++ generateDeep html2
      generateDeep (Color c1 c2 c3 html) = "<span style=\"color:rgb(" ++ show c1 ++ ", " ++ show c2 ++ ", " ++ show c3 ++ ");\">" ++ generateDeep html ++ "</span>"
  countWords = count_wordsDeep
    where
      count_wordsDeep (Text s) = length $ words s
      count_wordsDeep (Bold html) = count_wordsDeep html
      count_wordsDeep (Italics html) = count_wordsDeep html
      count_wordsDeep (Underline html) = count_wordsDeep html
      count_wordsDeep (Url text html) = count_wordsDeep html
      count_wordsDeep (Size size html) = count_wordsDeep html
      count_wordsDeep (List htmls) = sum [count_wordsDeep html | html <- htmls]
      count_wordsDeep (Concat html1 html2) = count_wordsDeep html1 + count_wordsDeep html2
      count_wordsDeep (ConcatSpace html1 html2) = count_wordsDeep html1 + count_wordsDeep html2
      count_wordsDeep (Color c1 c2 c3 html) = count_wordsDeep html

ex1' :: HTML
ex1' = text "hola soy un texto sin formato"

ex2' :: HTML
ex2' = (bold . text) "hola soy bold" <+> (underline . bold . text) "y subrayado"

ex3' :: HTML
ex3' =
  ex1' <-> size 35 (ex2' <+> text "y grande")
    <-> text "consultar en:"
    <+> url "http://www.google.com" ((bold . text) "Google")

ex4' :: HTML
ex4' =
  text "la lista es:"
    <+> list [ex1', ex2', (italics . text) "y nada mas"]

ex5' :: HTML
ex5' = (bold . text) "Hola" <+> (color 255 0 0 . text) "soy un texto en rojo" <+> text "pero se me pasa."

main = do
  writeFile "ex1.html" (generate ex1)
  writeFile "ex2.html" (generate ex2)
  writeFile "ex3.html" (generate ex3)
  writeFile "ex4.html" (generate ex4)
  writeFile "ex5p.html" (generate ex5')
