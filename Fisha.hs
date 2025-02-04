import Data.List
import Data.Char
--1.1

testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c
    |a+b>c && a+c>b && b+c>a = True
    |otherwise = False

--testaTriangulo a b c = a + b > c && a + c > b && b + c > a

--1.2

areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c =
    sqrt(s*(s-a)*(s-b)*(s-c))
    where
    s=(a+b+c)/2

--1.3
metades :: [a]->([a],[a])
metades xs =
    (take n xs, drop n xs)
    where
    n=length xs `div` 2

--1.4
--a

mylast :: [a] -> a
mylast xs =
    head(reverse xs)

--b

myinit:: [a] -> [a]
myinit xs =
    take(length xs -1) xs

--1.5

--a 

binom :: Integer -> Integer -> Integer
binom n k =
    product [1 .. n] `div` (product [1 .. k] * product [1 .. (n-k)])

--b

binom' :: Integer -> Integer-> Integer
binom' n k
    |k < (n-k) = product[(n-k+1) .. n] `div` product [1 .. k] 
    | otherwise = product [(k+1) .. n] `div` product [1 .. (n-k)]

--1.6

raizes :: Float -> Float -> Float -> (Float, Float)
raizes a b c =
    (((-b + delta)/(2*a)) , ((-b - delta)/(2*a)))
    where 
        delta = sqrt (b*b - 4*a*c)
    

--1.9

classifica :: Int -> String
classifica grade 
    |grade < 0 || grade > 20 = "Erro"
    |grade <= 9 = "reprovado"
    |grade>= 10 && grade<=12 = "suficiente"
    |grade>= 13 && grade<=15 = "bom"
    |grade>= 16 && grade<=18 = "muito bom"
    |grade>= 19 && grade<=20 = "muito bom com distinção"

--1.10

classifica' :: Float -> Float -> String
classifica' w h 
    |imc < 18.3 = "baixo peso"
    |imc>=18.5 && imc<25 = "peso normal"
    |imc>=25 && imc<30 = "excesso de peso"
    |imc>=30 =  "obsesidade"
    where imc=w/(h*h)

--1.11

--a
max, min :: Ord a => a -> a -> a
max x y = if x>=y then x else y
min x y = if x<=y then x else y

max3,min3 :: Ord a => a -> a -> a -> a
max3 x y z =
    if x>=y && x>=z then x
    else if y>=x && y>=z then y
    else z

min3 x y z =
    if x<=y && x<=z then x
    else if y<=x && y<=z then y
    else z

--b
max3',min3' :: Ord a => a -> a -> a -> a

max3' x y z =
    max2' x (max2' y z)
        where max2' c d 
                | c>=d = c
                | otherwise = d

min3' x y z =
    min2' x (min2' y z)
        where min2' c d
                | c<=d = c
                | otherwise = d

--1.12  

xor :: Bool -> Bool -> Bool
xor a b 
    |a == b = False
    |otherwise = True

--1.13

safetail :: [a] -> [a]
safetail [] = []
safetail xs = tail xs

--1.14

--a
curta :: [a] -> Bool
curta xs 
    |length xs <3 = True
    |otherwise = False

--b
curta' ::[a] -> Bool
curta' [] = True
curta' [_] =True
curta' [_,_] =True
curta' _ = False

--1.15

--a

mediana :: Ord a => a -> a -> a -> a
mediana a b c = sort [a, b, c] !! 1


mediana' :: (Num a, Ord a) => a -> a -> a -> a
mediana' a b c = (a+b+c) - min a (min b c) - max a (max b c)
    where min x y = if x<y then x else y
          max x y = if x>y then x else y


--2.1

--a

myand :: [Bool ] -> Bool
myand [] = False
myand [x] = x
myand (x:xs) =
    x && myand xs

--b

myor :: [Bool] -> Bool
myor [] = False
myor [x] = x
myor (x:xs) =
    x || myor xs

--c

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat [x] = x
myconcat (x:xs) =
    x ++ myconcat xs

--d

myreplicate :: Int -> a -> [a] 
myreplicate 0 _ = []
myreplicate n x = [x] ++ myreplicate (n-1) x

--e

(!!!) :: [a] -> Int -> a
(!!!) [] _ = error "empty list"
(!!!) l 0 = head l
(!!!) (x:xs) n = (!!!) xs (n-1)

--f
myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem n (x:xs) =
    if n==x then True
    else myelem n xs

--2.2

myintersperse :: a -> [a] -> [a]
myintersperse _ [] = []
myintersperse _ [x] = [x]
myintersperse n (x:xs) =
    [x] ++ [n] ++ myintersperse n xs

--2.3

mymdc :: Integer -> Integer -> Integer 
mymdc a b =
    if b==0 then a
    else mymdc b (a `mod` b)

--2.4

--a

myinsert :: Ord a => a -> [a] -> [a] 
myinsert a [] = [a]
myinsert a (x:xs)
    |a>=x =x : myinsert a xs
    |otherwise = a : x : xs

--b

myord :: Ord a => [a] ->[a]
myord [] =[]
myord (x:xs)=
    myinsert x (myord xs)

--2.5

--a

myminimum :: Ord a => [a] -> a
myminimum [x] = x
myminimum (x:xs) =
    if value < x then value else x
    where value = myminimum xs

--b

mydelete :: Eq a => a -> [a] -> [a]
mydelete _ [] = []
mydelete n (x:xs)
    |n == x = xs
    |otherwise = x : mydelete n xs

--c

ssort :: Ord a => [a] -> [a] 
ssort [] = []
ssort xs =
    m : ssort(mydelete m xs)
    where m = myminimum xs

--2.6
expression = sum ( map (^2) [1..100])

--2.7

--a

aprox :: Int -> Double 
aprox n =
    sum [((-1) ^ fromIntegral x) / fromIntegral (2 * x + 1) | x <- [0..n]] * 4

--b 

aprox' :: Int -> Double
aprox' n =
    sqrt(12*sum[((-1)^ fromIntegral x) / (fromIntegral(x+1)^2) | x <- [0..n]])

--2.8
dotprod :: [Float] -> [Float] -> Float
dotprod x y =
    sum(zipWith (*) x y)

--2.9

divprop :: Integer -> [Integer ] 
divprop n = [x | x <- [1..(n-1)] , n `mod` x == 0]

--2.10

perfeitos :: Integer -> [Integer]
perfeitos n = [x | x <- [1..n] , sum(divprop x) == x]

--2.11

pitagoricos :: Integer -> [(Integer , Integer , Integer )] 
pitagoricos n = [(x , y , z)| x <- [1..n], y <- [1..n], z <- [1..n], x*x + y*y == z*z]

--2.12

primo :: Integer -> Bool
primo n 
    |(divprop n) == [1] = True
    |otherwise = False

--2.13

mersennes :: [Int]
mersennes = [2^x-1 | x <- [2..30] , primo(2^x-1)]

--2.14

pascalline :: Integer -> [Integer]
pascalline n =[binom n k | k <- [0..n]]

pascal :: Integer -> [[Integer ]]
pascal n = [pascalline i | i<- [0..n]]

--2.15
--rezolvat de prof

--2.16

myconcat2 :: [[a]] -> [a]
myconcat2 lists = [value | list <- lists, value <- list]

myreplicate2 :: Int -> a -> [a]
myreplicate2 n list = [list | _ <- [1..n]]


mybangbang :: Int -> [a] -> a
mybangbang _ [] = error "Empty list"
mybangbang n list =head [value | (value, index) <- zip list [1..(length list)], index == n]

--2.17

forte :: String -> Bool 
forte string = length string >= 8 && letterM && number && letterM
    where 
        func filtro string = length [c | c <- string, filtro c] >= 1
        letterM = func isUpper string 
        letterm = func isLower string
        number = func isDigit string

--2.18

--a

mindiv :: Int -> Int
mindiv n = 
    if length list > 0 then head list else n
    where list = [x | x <- [2..floor(sqrt (fromIntegral n))], n `mod` x == 0]

--b

primo2 :: Int -> Bool
primo2 n = n > 1 && mindiv n == n

--2.19

mynub :: Eq a => [a] -> [a]
mynub (x:xs) = x : mynub (filter (/= x) xs)

--2.20
--2.21
--2.22
--2.23
--2.24

--3.1

--map f (filter p xs)

--3.2

dec2int :: [Int] -> Int
dec2int list = foldl (\acc x -> acc*10 + x) 0 list

--3.3

myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 
myzipWith func [] [] = []
myzipWith func [] a = []
myzipWith func a [] = []
myzipWith func (x:xs) (y:ys) = [func x y] ++ myzipWith func xs ys

--3.4

myisort :: Ord a => [a] -> [a] 
myisort list = foldl (\acc x -> insert x acc) [] list

--3.5

mymaximuml, myminimuml :: Ord a => [a] -> a

mymaximuml list = foldl1(\acc x -> if x > acc then x else acc) list 

myminimuml list = foldl1(\acc x -> if x < acc then x else acc) list

mymaximumr, myminimumr :: Ord a => [a] -> a

mymaximumr list = foldr1(\x acc -> if x > acc then x else acc) list 

myminimumr list = foldr1(\x acc -> if x < acc then x else acc) list

--3.6

mdc :: Int -> Int -> Int
mdc a b = fst( until (\(a, b) -> b == 0) (\(a, b) -> (b, mod a b)) (a, b))

--3.7

--3.8

--3.9

--3.10

--3.11

--4.1
