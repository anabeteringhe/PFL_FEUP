type Match = ((String,String), (Int,Int))
type MatchDay = [Match]
type League = [MatchDay]

myLeague :: League
myLeague = [
  [(("Porto","Sporting"),(2,2)),(("Benfica","Vitoria SC"),(4,0))],
  [(("Porto","Benfica"),(5,0)),(("Vitoria SC","Sporting"),(3,2))],
  [(("Vitoria SC","Porto"),(1,2)),(("Sporting","Benfica"),(2,1))]
  ]

--1 facut de mine

winner :: Match -> String
winner ((hteam, ateam) , (hgoal,agoal))=
    if hgoal==agoal then "draw"
    else if hgoal>agoal then hteam
    else ateam

--2 nici o sansa

matchDayScore :: String -> MatchDay -> Int
matchDayScore _ [] = 0
matchDayScore team (x:xs)
    |(team==hteam || team==ateam) && win==team = 3
    |(team==hteam || team==ateam) && win=="draw" = 1
    |team==hteam || team==ateam = 0
    |otherwise = matchDayScore team xs
    where 
        win = winner x
        (hteam,ateam) = fst x

--helping code

leagueScore :: String -> League -> Int
leagueScore t = foldr (\d acc -> matchDayScore t d + acc) 0

sortByCond :: Ord a => [a] -> (a -> a -> Bool) -> [a]
sortByCond [] _ = []
sortByCond [x] _ = [x]
sortByCond l cmp = merge (sortByCond l1 cmp) (sortByCond l2 cmp) cmp
  where (l1 ,l2) = splitAt (div (length l) 2) l

merge :: Ord a => [a] -> [a] -> (a -> a -> Bool) -> [a]
merge [] l _ = l
merge l [] _ = l
merge (x:xs) (y:ys) cmp
  | cmp x y = x:(merge xs (y:ys) cmp)
  | otherwise = y:(merge (x:xs) ys cmp)

--3 --nici o sansa
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs)=x:nub(filter (/=x) xs)


ranking:: League -> [(String,Int)]
ranking [] = []
ranking league = sortByCond teamScores cmp
  where
    allTeams = nub( concatMap (\matchday -> concatMap (\((hteam, ateam), _) -> [hteam, ateam]) matchday) league)
    teamScores = [(team, leagueScore team league) | team <- allTeams]
    cmp (team1, score1) (team2, score2)
      | score1 > score2 = True
      | score1 < score2 = False
      | otherwise = team1 < team2

--4 nope

numMatchDaysWithDraws :: League -> Int
numMatchDaysWithDraws = length . filter (any((=="draw").winner))

--5 nope

bigWins :: League -> [(Int,[String])]
bigWins league =
  [(i,[winner match | match@(_, (score1,score2)) <- matchDay, abs(score1-score2)>=3 ])|(i,matchDay)<-zip[1..]league]

--6 i understand shit

