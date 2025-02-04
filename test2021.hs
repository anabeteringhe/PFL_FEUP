
type Matriz = [[Int]]
type Vector = [Int]

--1

maxpos :: [Int] -> Int
maxpos [] = 0
maxpos [x] = x
--maxpos list = foldl (\acc x -> if x>acc then x else acc) 0 list
maxpos (x:xs) 
    |x>next = x
    |otherwise = next
    where next = maxpos xs

--2

dups :: [a] -> [a]
dups [] = []
dups (x:y:xs) = x : x : y : dups xs

--3

transforma :: String -> String
transforma [] = []
transforma (x:xs) =
    if x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u' then x : 'p' : x : transforma xs
    else x : transforma xs

--4

transposta :: Matriz -> Matriz
transposta [] = []
transposta list = [head x | x <- list] : transposta [tail x | x <- list, tail x /= []]

--5

prodInterno :: Vector -> Vector -> Int
prodInterno [] [] = 0
prodInterno (x:xs) (y:ys)= x * y + prodInterno xs ys

--6

prodMat :: Matriz -> Matriz -> Matriz
prodMat mat1 mat2 = [[prodInterno x y | y <-transposta mat2] | x <- mat1]


--9
f :: (a -> b -> c) -> b -> a -> c
f g b a = g a b