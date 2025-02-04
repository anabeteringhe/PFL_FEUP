type Species = (String, Int)

type Zoo = [Species]

--1

isEndangered :: Species -> Bool
isEndangered (name, nr) =
    if nr <= 100 then True else False

--2

updateSpecies :: Species -> Int -> Species
updateSpecies (name, count) newcount = (name , count+newcount)

--3 needed help

filterSpecies :: Zoo -> (Species -> Bool) -> Zoo
filterSpecies [] _ = []
filterSpecies (x:xs) func =
    if func x then x : filterSpecies xs func else filterSpecies xs func

--4 needed help

countAnimals :: Zoo -> Int
countAnimals zoo = sum ( map snd zoo )
--countAnimals animals = sum(map (\(name,count) -> count) animals)

--5 needed help

substring :: (Integral a) => String -> a -> a -> String
substring name start end = [name !! fromIntegral i | i <- [start, end]]

--6 nope

hasSubstr :: String -> String -> Bool
hasSubstr str substr = any (isPrefix substr) (tails str)
  where
    isPrefix :: String -> String -> Bool
    isPrefix [] _ = True
    isPrefix _ [] = False
    isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

    tails :: String -> [String]
    tails [] = []
    tails s@(_:xs) = s : tails xs

--7 nope

sortSpeciesWithSubstr :: Zoo -> String -> (Zoo, Zoo)
sortSpeciesWithSubstr zoo string =
    (withString,withoutString)
    where 
        withString = [species | species@(name,_)<-zoo, hasSubstr name string]
        withoutString = [species | species@(name,_)<-zoo, not (hasSubstr name string)]

--8

rabbits :: (Integral a) => [a]
rabbits = 2 : 3 : zipWith (+) rabbits (tail rabbits)

--9

rabbitYears :: (Integral a) => a -> Int
rabbitYears year = length ([y | y<-(take (fromIntegral year) rabbits), y<year])