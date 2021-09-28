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

{- newtype SHTML1 = H String
  deriving (Show)

instance DSL_HTML SHTML1 where
  text t = H t
  bold (H t) = H ("<b>" ++ t ++ "</b>")
  italics (H t) = H ("<i>" ++ t ++ "</i>")
  underline (H t) = H ("<ins>" ++ t ++ "</ins>")
  url url (H t) = H ("<a href=\"" ++ url ++ "\">" ++ t ++ "</a>")
  size s (H t) = H ("<span style=\"font-size:" ++ show s ++ "px\">" ++ t ++ "</span>")
  list ts = H ("<ul>" ++ concatMap (\(H t) -> "<li>" ++ t ++ "</li>") ts ++ "</ul>")
  (<->) (H t1) (H t2) = H (t1 ++ "<br>" ++ t2)
  (<+>) (H t1) (H t2) = H (t1 ++ " " ++ t2)
  generate (H t) = t
  countWords _ = undefined -}

newtype SHTML = H (Int, String)
    deriving (Show)

instance DSL_HTML SHTML where
    text x = H (length $ words x, x)
    bold (H x) = H (fst x, "<b>" ++ snd x ++ "</b>")
    italics (H x) = H (fst x, "<i>" ++ snd x ++ "</i>")
    underline (H x) = H (fst x, "<ins>" ++ snd x ++ "</ins>")
    url url (H x) = H (fst x, "<a href=\"" ++ url ++ "\">" ++ snd x ++ "</a>")
    size s (H x) = H (fst x, "<span style=\"font-size:" ++ show s ++ "px\">" ++ snd x ++ "</span>")
    list xs = H (foldl' (\sum (H (x,_)) -> sum + x) 0 xs, "<ul>" ++ concatMap (\(H (_, t)) -> "<li>" ++ t ++ "</li>") xs ++ "</ul>")
    (<->) (H x1) (H x2) = H (fst x1 + fst x2, snd x1 ++ "<br>" ++ snd x2)
    (<+>) (H x1) (H x2) = H (fst x1 + fst x2, snd x1 ++ " " ++ snd x2)
    generate (H x) = snd x 
    countWords (H x) = fst x
    color r g b (H x) = H (fst x, "<span style=\"color: rgb(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")\">" ++ snd x ++ "</span>") 

    -- generate (H t) = "<html>\n  <head></head>\n  <body>" ++ t ++ "\n  </body>\n</html>"

ex1 :: SHTML
ex1 = text "hola soy un texto sin formato"

ex2 :: SHTML
ex2 = (bold . text) "hola soy bold" <+> (underline . bold . text) " y subrayado"

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

data DHTML where
  Text :: String -> DHTML
  Bold :: DHTML -> DHTML
  Italics :: DHTML -> DHTML
  Underline :: DHTML -> DHTML
  Url :: DHTML -> DHTML -> DHTML
  List :: [DHTML] -> DHTML
  Size :: Int -> DHTML -> DHTML
  Concat :: DHTML -> DHTML -> DHTML
  ConcatSpace :: DHTML -> DHTML -> DHTML

instance DSL_HTML DHTML where
  text s = Text s
  bold html = Bold html
  italics html = Italics html
  underline html = Underline html
  url string html = Url (text string) html
  size tam html = Size tam html
  list htmls = List htmls
  (<->) html1 html2 = Concat html1 html2
  (<+>) html1 html2 = ConcatSpace html1 html2
  generate = generateDeep
    where
      generateDeep (Text s) = s
      generateDeep (Bold html) = "<b>" ++ generateDeep html ++ "</b>"
      generateDeep (Italics html) = "<i>" ++ generateDeep html ++ "</i>"
      generateDeep (Underline html) = "<ins>" ++ generateDeep html ++ "</ins>"
      generateDeep (Url text html) = "<a href=\"" ++ generateDeep text ++ "\">" ++ generateDeep html ++ "</a>"
      generateDeep (Size size html) = "<span style=\"font-size:" ++ show size ++ "px\">" ++ generateDeep html ++ "</span>"
      generateDeep (List htmls) = "<ul>" ++ concat ["<li>" ++ generateDeep html ++ "</li>" | html <- htmls] ++ "</ul>"
      generateDeep (Concat html1 html2) = generateDeep html1 ++ "<br/>" ++ generateDeep html2
      generateDeep (ConcatSpace html1 html2) = generateDeep html1 ++ " " ++ generateDeep html2
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

ex1' :: DHTML
ex1' = text "hola soy un texto sin formato"

ex2' :: DHTML
ex2' = (bold . text) "hola soy bold" <+> (underline . bold . text) "y subrayado"

ex3' :: DHTML
ex3' =
  ex1' <-> size 35 (ex2' <+> text "y grande")
    <-> text "consultar en:"
    <+> url "http://www.google.com" ((bold . text) "Google")

ex4' :: DHTML
ex4' =
  text "la lista es:"
    <+> list [ex1', ex2', (italics . text) "y nada mas"]

main = do
  print (generate ex1)
  print (generate ex2)
  print (generate ex3)
  print (generate ex4)
