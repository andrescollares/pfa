import Ej5

ex1 = text "hola soy un texto sin formato"
ex2 = (bold . text) "hola soy bold" <+> (underline . bold . text) "y subrayado"
ex3 = ex1 <-> size 35 (ex2 <+> text "y grande") <->
      text "consultar en:" <+>
      url "http://www.google.com"((bold . text) "Google")
ex4 = text "la lista es:" <+>
      list [ex1, ex2, (italics . text) "y nada mas"]

main = do return $ generate ex1
