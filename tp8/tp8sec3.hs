data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA

evalExpA :: ExpA -> Int
evalExpA (Cte n)       = n
evalExpA (Suma e1 e2)  = evalExpA e1 + evalExpA e2
evalExpA (Prod e1 e2)  = evalExpA e1 * evalExpA e2

simplificarExpA :: ExpA -> ExpA
simplificarExpA (Cte n)       = Cte n
simplificarExpA (Suma e1 e2)  = simplificarSuma (simplificarExpA e1) (simplificarExpA e2)
simplificarExpA (Prod e1 e2)  = simplificarProd (simplificarExpA e1) (simplificarExpA e2)

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma (Cte 0) e2     = e2
simplificarSuma e1 (Cte 0)     = e1
simplificarSuma (Cte n1) (Cte n2) = Cte (n1 + n2)
simplificarSuma e1 e2          = Suma e1 e2

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Cte 0) _      = Cte 0
simplificarProd _ (Cte 0)      = Cte 0
simplificarProd (Cte 1) e2     = e2
simplificarProd e1 (Cte 1)     = e1
simplificarProd (Cte n1) (Cte n2) = Cte (n1 * n2)
simplificarProd e1 e2          = Prod e1 e2

cantidadDeSumaCero :: ExpA -> Int
cantidadDeSumaCero (Cte _) = 0
cantidadDeSumaCero (Prod e1 e2) = cantidadDeSumaCero e1 + cantidadDeSumaCero e2
cantidadDeSumaCero (Suma e1 e2) = unoSiAmbosCero e1 e2 + cantidadDeSumaCero e1 + cantidadDeSumaCero e2


unoSiAmbosCero :: ExpA -> ExpA -> Int
unoSiAmbosCero (Cte 0) (Cte 0) = 1
unoSiAmbosCero _ _             = 0