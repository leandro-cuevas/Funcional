data Dungeon a = Habitacion a | Pasaje (Maybe a) (Dungeon a) | Bifurcacion (Maybe a) (Dungeon a) (Dungeon a)
data Tesoro = Cofre | Oro | Joyas
data VariasCosas a b = Objeto a | Criatura b
data Monstruo = Gargola | Dragon | Troll


cantidadDeBifurcaciones:: Dungeon a -> Int
cantidadDeBifurcaciones (Habitacion _)         = 0
cantidadDeBifurcaciones (Pasaje _ d)           = cantidadDeBifurcaciones d
cantidadDeBifurcaciones (Bifurcacion _ d1 d2)  = 1 + cantidadDeBifurcaciones d1 + cantidadDeBifurcaciones d2 

cantidadDePuntosInteresantes:: Dungeon a -> Int
cantidadDePuntosInteresantes (Habitacion _)                   = 1
cantidadDePuntosInteresantes (Pasaje talVezAlgo d)            = contarCosa talVezAlgo + cantidadDeBifurcaciones d
cantidadDePuntosInteresantes (Bifurcacion talVezAlgo d1 d2)   = contarCosa talVezAlgo + cantidadDeBifurcaciones d1 +cantidadDeBifurcaciones d2

contarCosa:: Maybe a -> Int
contarCosa (Just _) = 1
contarCosa _        = 0

descontarCosa:: Maybe a -> Int
descontarCosa Nothing  = 1
descontarCosa _        = 0

cantidadDePuntosVacios:: Dungeon a -> Int
cantidadDePuntosVacios (Habitacion _)                   = 0
cantidadDePuntosVacios (Pasaje talVezAlgo d)            = descontarCosa talVezAlgo + cantidadDePuntosVacios d
cantidadDePuntosVacios (Bifurcacion talVezAlgo d1 d2)   = descontarCosa talVezAlgo + cantidadDePuntosVacios d1 + cantidadDePuntosVacios d2

cantidadDePuntosCon:: Eq a => a -> Dungeon a -> Int
cantidadDePuntosCon a1 (Habitacion a2)       = sumaSiSonIguales (a1==a2)
cantidadDePuntosCon a1 (Pasaje a2 d)         = sumaSiSonIguales (maybeIgual a1 a2)+ (cantidadDePuntosCon a1 d)
cantidadDePuntosCon a1 (Bifurcacion a2 d1 d2)= sumaSiSonIguales (maybeIgual a1 a2) + (cantidadDePuntosCon a1 d1) + (cantidadDePuntosCon a1 d2)

sumaSiSonIguales:: Bool -> Int
sumaSiSonIguales True = 1
sumaSiSonIguales False= 0

maybeIgual :: Eq a => a -> Maybe a -> Bool
maybeIgual _ Nothing  = False
maybeIgual x (Just y) = x == y

esLineal:: Dungeon a-> Bool
esLineal (Habitacion a)         = True
esLineal (Bifurcacion a d1 d2)  = False
esLineal (Pasaje a d)           = esLineal d

llenoDe:: Eq a => a -> Dungeon a -> Bool
llenoDe a1 (Habitacion a2)          = a1 == a2
llenoDe a1 (Pasaje a2 d)            = maybeIgual a1 a2 && llenoDe a1 d
llenoDe a1 (Bifurcacion a2 d1 d2)   = maybeIgual a1 a2 && llenoDe a1 d1 && llenoDe a1 d2
