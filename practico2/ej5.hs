class DSL_HTML t where
    text :: String -> t
    bold :: t -> t
    italics :: t -> t
    underline :: t -> t
    url :: String -> t -> t
    size :: Int -> t -> t -- revisar este, el HTML mantiene un campo largo o como?
    list :: [h] -> t -- Utilizar el tipo lista de HTML
    (<->) :: t -> t -> t -- Separa usando un salto de linea
    (<+>) :: t -> t -> t -- Separa usando un espacio en blanco
    generate :: t -> String


-- shallow embedded

newtype SHTML = String -> Bool 

instance DSL_HTML SHTML where
    
    bold txt = "<b>" ++ txt ++ "</b>"
    italics txt = "<i>" ++ txt ++ "</i>"
    underline txt = "<u>" ++ txt ++ "</u>"
    url url txt = "<a href='" ++ url ++ "'>" ++ txt ++ "</a>"
    
    list txts = "<ul>" ++ (concatMap (\txt -> "<li>" ++ ) (t a)) ++ "</ul>"

