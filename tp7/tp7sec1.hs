data Pizza = Prepizza | Capa Ingrediente Pizza
data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamon | Queso | Salsa

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
cantidadDeAceitunasEnIngrediente (Aceitunas n) = n
cantidadDeAceitunasEnIngrediente _             = 0

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza     = Prepizza
duplicarAceitunas (Capa ing p) = Capa (duplicarAceitunaEnIngrediente ing) (duplicarAceitunas p)

duplicarAceitunaEnIngrediente :: Ingrediente -> Ingrediente
duplicarAceitunaEnIngrediente (Aceitunas n) = Aceitunas (n*2)
duplicarAceitunaEnIngrediente ing           = ing

sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza     = Prepizza
sinLactosa (Capa ing p) = procesarCapaSinLactosa ing (sinLactosa p)

procesarCapaSinLactosa :: Ingrediente -> Pizza -> Pizza
procesarCapaSinLactosa Queso p = p
procesarCapaSinLactosa ing   p = Capa ing p

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
conDescripcionMejorada Prepizza     = Prepizza
conDescripcionMejorada (Capa ing p) = juntarAceitunas ing (conDescripcionMejorada p)

juntarAceitunas :: Ingrediente -> Pizza -> Pizza
juntarAceitunas (Aceitunas n1) p = Capa (Aceitunas (n1 + sumaAceitunas p)) (restoDespuesAceitunas p)
juntarAceitunas ing p             = Capa ing p

sumaAceitunas :: Pizza -> Int
sumaAceitunas (Capa (Aceitunas n) _) = n
sumaAceitunas _                       = 0

restoDespuesAceitunas :: Pizza -> Pizza
restoDespuesAceitunas (Capa (Aceitunas _) resto) = resto
restoDespuesAceitunas p                           = p

p = Capa Queso (Capa (Aceitunas 3) (Capa (Aceitunas 2) (Capa Jamon Prepizza)))

