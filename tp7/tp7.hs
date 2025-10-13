data Pizza = Prepizza | Capa Ingrediente Pizza
data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamón | Queso | Salsa

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza               = 0
cantidadDeAceitunas (Capa (Aceitunas n) p) = n + cantidadDeAceitunas p
cantidadDeAceitunas (Capa _ p )            =  cantidadDeAceitunas p

cantidadDeAceitunas' :: Pizza -> Int
cantidadDeAceitunas' Prepizza     = 0
cantidadDeAceitunas' (Capa ing p) = cantidadDeAceitunasEnIngrediente ing + cantidadDeAceitunas' p

cantidadDeAceitunasEnIngrediente :: Ingrediente -> Int
cantidadDeAceitunasEnIngrediente Aceitunas n = n
cantidadDeAceitunasEnIngrediente _           = 0

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza     = Prepizza
duplicarAceitunas (Capa ing p) = Capa (duplicarAceitunaEnIngrediente ing) (duplicarAceitunas p)

duplicarAceitunaEnIngrediente :: Ingrediente -> Ingrediente
duplicarAceitunaEnIngrediente Aceitunas n = Aceitunas (n*2)
duplicarAceitunaEnIngrediente ing         = ing

sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza     = Prepizza
sinLactosa (Capa ing p) = if (esIngredienteLacteo ing)
                          then (sinLactosa p)
                          else (Capa ing (sinLactosa p))

esIngredienteLacteo :: Ingrediente -> Bool
esIngredienteLacteo Queso = True
esIngredienteLacteo _     = False

sinLactosa' :: Pizza -> Pizza
sinLactosa' Prepizza       = Prepizza
sinLactosa' (Capa Queso p) = sinLactosa p
sinLactosa' (Capa ing p)   = Capa ing (sinLactosa p)

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa Prepizza       = True
aptaIntolerantesLactosa (Capa Queso p) = False
aptaIntolerantesLactosa (Capa _ p)     = aptaIntolerantesLactosa p

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada Prepizza = Prepizza
conDescripcionMejorada (Capa (Aceitunas n1) (Capa (Aceitunas n2) p)) =  conDescripcionMejorada (Capa (Aceitunas (n1+n2)) p)
conDescripcionMejorada (Capa ing p) = Capa ing (conDescripcionMejorada p)

conDescripcionMejorada' :: Pizza -> Pizza
conDescripcionMejorada' Prepizza     = Prepizza
conDescripcionMejorada' (Capa ing p) = juntar ing (conDescripcionMejorada p)
 
-- Subtarea: recibe un ingrediente y una pizza ya procesada
juntar :: Ingrediente -> Pizza -> Pizza
juntar (Aceitunas n1) (Capa (Aceitunas n2) resto) = Capa (Aceitunas (n1+n2)) resto
juntar ing resto                                 = Capa ing resto