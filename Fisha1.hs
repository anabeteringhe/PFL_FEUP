import Data.Graph (graphFromEdges)
import Text.XHtml (height)
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