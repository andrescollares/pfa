{-# LANGUAGE GADTs #-}

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

-- shallow embedded

-- newtype SHTML = String -> Bool

-- instance DSL_HTML SHTML where
--   bold txt = "<b>" ++ txt ++ "</b>"
--   italics txt = "<i>" ++ txt ++ "</i>"
--   underline txt = "<u>" ++ txt ++ "</u>"
--   url url txt = "<a href='" ++ url ++ "'>" ++ txt ++ "</a>"

-- list txts = "<ul>" ++ (concatMap (\txt -> "<li>" ++ ) (t a)) ++ "</ul>"

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

instance DSL_HTML HTML where
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
      generateDeep (Size size html) = "<span style=\"font-size:" ++ show size ++ "\">" ++ generateDeep html ++ "</span>"
      generateDeep (List htmls) = "<ul>" ++ concat ["<li>" ++ generateDeep html ++ "</li>" | html <- htmls] ++ "</ul>"
      generateDeep (Concat html1 html2) = generateDeep html1 ++ "<br/>" ++ generateDeep html2
      generateDeep (ConcatSpace html1 html2) = generateDeep html1 ++ " " ++ generateDeep html2

ex1 :: HTML
ex1 = text "hola soy un texto sin formato"

ex2 :: HTML
ex2 = (bold . text) "hola soy bold" <+> (underline . bold . text) "y subrayado"

ex3 :: HTML
ex3 =
  ex1 <-> size 35 (ex2 <+> text "y grande")
    <-> text "consultar en:"
    <+> url "http://www.google.com" ((bold . text) "Google")

ex4 :: HTML
ex4 =
  text "la lista es:"
    <+> list [ex1, ex2, (italics . text) "y nada mas"]

main = do
  writeFile "ex1.html" (generate ex1)
  writeFile "ex2.html" (generate ex2)
  writeFile "ex3.html" (generate ex3)
  writeFile "ex4.html" (generate ex4)