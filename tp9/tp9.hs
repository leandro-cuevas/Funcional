data EA = Const Int | BOp BinOp EA EA
data BinOp = Sum | Mul
data ExpA = Cte Int| Suma ExpA ExpA | Prod ExpA ExpA
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
data Arbol a b = Hoja b | Nodo a (Arbol a b) (Arbol a b)

evalEA :: EA -> Int
evalEA (Const n) = n
evalEA (BOp op ea1 ea2) = evalOp op (evalEA ea1) (evalEA ea2)

evalOp :: BinOp -> Int -> Int -> Int
evalOp Sum n1 n2 = n1 + n2
evalOp Mul n1 n2 = n1 * n2

ea2ExpA :: EA -> ExpA
ea2ExpA (Const n) = Cte n
ea2ExpA (BOp op ea1 ea2) = ea2Op2ExpA op (ea2ExpA ea1) (ea2ExpA ea2)

ea2Op2ExpA :: BinOp -> ExpA -> ExpA -> ExpA
ea2Op2ExpA Sum = Suma
ea2Op2ExpA Mul = Prod

expA2ea :: ExpA -> EA
expA2ea (Cte n) = Const n
expA2ea (Suma ea1 ea2) = BOp Sum (expA2ea ea1) (expA2ea ea2)
expA2ea (Prod ea1 ea2) = BOp Mul (expA2ea ea1) (expA2ea ea2)

cantidadDeHojas :: Arbol a b -> Int
cantidadDeHojas (Hoja _) = 1
cantidadDeHojas (Nodo _ izq der) = cantidadDeHojas izq + cantidadDeHojas der

cantidadDeNodos :: Arbol a b -> Int
cantidadDeNodos (Hoja _) = 0
cantidadDeNodos (Nodo _ izq der) = 1 + cantidadDeNodos izq + cantidadDeNodos der

cantidadDeConstructores :: Arbol a b -> Int
cantidadDeConstructores (Hoja _) = 1
cantidadDeConstructores (Nodo _ izq der) = 1 + cantidadDeConstructores izq + cantidadDeConstructores der

ea2Arbol :: EA -> Arbol BinOp Int
ea2Arbol (Const x) 	  = Hoja x
ea2Arbol (BOp op ea1 ea2) = Nodo op (ea2Arbol ea1) (ea2Arbol ea2)


sumarT :: Tree Int -> Int
sumarT EmptyT         = 0
sumarT(NodeT n t1 t2) = n + sumarT t1 + sumarT t2

sizeT :: Tree a -> Int
sizeT  EmptyT         = 0
sizeT (NodeT _ t1 t2) = 1 + sizeT t1 + sizeT t2

anyT :: (a -> Bool) -> Tree a -> Bool
anyT f EmtyT 	       = False
anyT f (NodeT x a1 a2) = f x || anyT a1 || anyT a2

countT :: (a -> Bool) -> Tree a -> Int
countT f EmtyT 	         = 0
countT f (NodeT x a1 a2) = unoSiCeroSino f x + countT a1 + countT a2

countLeaves :: Tree a -> Int
countLeaves EmptyT          = 0
countLeaves (NodeT _ a1 a2) = unoSiCeroSino (esEmptyT a1 && esEmptyT a2)  + countLeaves a1 + countLeaves a2

esEmptyT :: Tree a -> Bool
esEmptyT EmptyT = True
esEmptyT _      = False



