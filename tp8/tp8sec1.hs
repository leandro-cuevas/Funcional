length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + length xs

sum :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs 

product :: [Int] -> Int
product []     = 1
product (x:xs) = x * product xs 

concat :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ concat xss

elem :: Eq a => a -> [a] -> Bool
elem x []       = False
elem x1 (x2:xs) = x1 == x2 || elem x1 xs

all :: (a -> Bool) -> [a] -> Bool
all f []     = True
all f (x:xs) = f x && all f xs

any :: (a -> Bool) -> [a] -> Bool
any f []     = False
any f (x:xs) = f x || any f xs

count :: (a -> Bool) -> [a] -> Int
count _ []     = 0
count f (x:xs) = uniSiCeroSino (f x) + count f xs

uniSiCeroSino :: Bool -> Int
uniSiCeroSino True = 1
uniSiCeroSino _    = 0

subset :: Eq a => [a] -> [a] -> Bool
subset [] _        = True
subset (x:xs1) xs2 = elem x xs2 && subset xs1 xs2

(++) :: [a] -> [a] -> [a]
(++) [] ys     = xs
(++) (x:xs) ys = x : (xs ++ ys)

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip [] ys = []
zip xs [] = []
zip (x:xs) (y:ys) = (x,y) : (zip xs ys)

unzip :: [(a,b)] -> ([a],[b])
unzip [] = ([], [])
unzip ((a,b):xs) = (a : fst (unzip xs), b : snd (unzip xs))

null :: [a] -> Bool
null []     = True
null (_:_)  = False
