type Nombre = String
data Planilla = Fin | Registro Nombre Planilla
data Equipo = Becario Nombre | Investigador Nombre Equipo Equipo Equipo

largoDePlanilla :: Planilla -> Int
largoDePlanilla Fin            = 0
largoDePlanilla (Registro n p) = 1 + largoDePlanilla p

esta :: Nombre -> Planilla -> Bool
esta _ Fin              = False
esta n1 (Registro n2 p) = n1 == n2 || esta n1 p

juntarPlanillas :: Planilla -> Planilla -> Planilla
juntarPlanillas Fin p2              = p2
juntarPlanillas (Registro n p1) p2  = Registro n (juntarPlanillas p1 p2)

nivelesJerarquicos :: Equipo -> Int
nivelesJerarquicos (Becario _)               = 1
nivelesJerarquicos (Investigador _ e1 e2 e3) = 1 + maximoEntreTres (nivelesJerarquicos e1) (nivelesJerarquicos e2) (nivelesJerarquicos e3)

maximoEntreTres :: Int -> Int -> Int-> Int
maximoEntreTres x y z = max x (max y z)

cantidadDeIntegrantes :: Equipo -> Int
cantidadDeIntegrantes (Becario _)                 = 1
cantidadDeIntegrantes (Investigador _ e1 e2 e3) = 1 + (cantidadDeIntegrantes e1) + (cantidadDeIntegrantes e2) + (cantidadDeIntegrantes e3)

planillaDeIntegrantes :: Equipo -> Planilla
planillaDeIntegrantes (Becario n)                = Registro n Fin
planillaDeIntegrantes (Investigador n e1 e2 e3 ) = Registro n (juntarPlanillas
                                                    (juntarPlanillas (planillaDeIntegrantes e1) (planillaDeIntegrantes e2)) (planillaDeIntegrantes e3))

