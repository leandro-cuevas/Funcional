data N = Z | S N

evalN :: N -> Int
evalN Z     = 0
evalN (S n) = 1 + evalN n

addN :: N -> N -> N
addN Z n     = n
addN (S n1) n2 = S (addN n1 n2)

prodN :: N -> N -> N
prodN Z _       = Z
prodN (S n1) n2 = addN n2 (prodN n1 n2)

int2N :: Int -> N
int2N 0 = Z
int2N n = S (int2N (n-1))

