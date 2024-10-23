import Data.Array qualified
import Data.Bits qualified
import Data.List qualified
import Data.List (permutations, minimumBy)
import Data.Maybe (fromJust, isJust)
import Data.Ord (comparing)

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String

type Path = [City]

type Distance = Int

type RoadMap = [(City, City, Distance)]

cities :: RoadMap -> [City]
cities roadmap = Data.List.nub [city | (city1, city2, _) <- roadmap, city <- [city1, city2]]

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadmap city1 city2 = any (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) roadmap

distance :: RoadMap -> City -> City -> Maybe Distance --not sure it's correct, but it's the best I could do
distance roadmap city1 city2 =
  case filter (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) roadmap of
    ((_, _, d) : _) -> Just d

adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent roadmap city = [(c2, d) | (c1, c2, d) <- roadmap, c1 == city] ++ [(c1, d) | (c1, c2, d) <- roadmap, c2 == city]

pathDistance :: RoadMap -> Path -> Maybe Distance --not working
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance roadmap (c1 : c2 : cs) = do
  d1 <- distance roadmap c1 c2
  d2 <- pathDistance roadmap (c2 : cs)
  return (d1 + d2)

rome :: RoadMap -> [City]
rome roadmap =
  let cityDegrees = [(city, length [(c1, c2, d) | (c1, c2, d) <- roadmap, city == c1 || city == c2]) | city <- cities roadmap]
      maxDegree = maximum (map snd cityDegrees)
   in [city | (city, degree) <- cityDegrees, degree == maxDegree]

reachableFrom :: RoadMap -> City -> [City]
reachableFrom roadmap city = dfs [city] []
  where
    dfs [] visited = visited
    dfs (current:stack) visited
        | current `elem` visited = dfs stack visited
        | otherwise = dfs (adjacentCities ++ stack) (current : visited)
      where
        adjacentCities = [c | (c, _) <- adjacent roadmap current]

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadmap = all (\city -> length (reachableFrom roadmap city) == length (cities roadmap)) (cities roadmap)

bfsPaths :: RoadMap -> City -> City -> [[(City, Distance)]]
bfsPaths roadmap start goal = bfs [(start, 0)] [] []
  where
    bfs [] _ paths = paths
    bfs ((current, dist):queue) visited paths
        | current == goal = bfs queue visited (((goal, dist):visited) : paths)
        | otherwise = bfs (queue ++ newPaths) (visited ++ [(current, dist)]) paths
      where
        adjacentCities = adjacent roadmap current
        newPaths = [(city, dist + d) | (city, d) <- adjacentCities, city `notElem` map fst visited]

shortestPath :: RoadMap -> City -> City -> [Path] --not working 
shortestPath roadmap start goal
  | start == goal = [[start]]
  | otherwise =
      let allPaths = bfsPaths roadmap start goal
          shortestDist = minimum (map (sum . map snd) allPaths)
      in map (map fst . reverse) $ filter (\p -> sum (map snd p) == shortestDist) allPaths

travelSales :: RoadMap -> Path --not working
travelSales roadmap =
    let allCities = cities roadmap
        allPaths = permutations allCities
        validPaths = filter (isCompletePath roadmap) allPaths
        distances = map (\p -> (p, fromJust (pathDistance roadmap (p ++ [head p])))) validPaths
    in fst $ minimumBy (\(_, d1) (_, d2) -> compare d1 d2) distances

isCompletePath :: RoadMap -> Path -> Bool
isCompletePath roadmap path = isJust (pathDistance roadmap path)


gTest1 :: RoadMap
gTest1 = [("7", "6", 1), ("8", "2", 2), ("6", "5", 2), ("0", "1", 4), ("2", "5", 4), ("8", "6", 6), ("2", "3", 7), ("7", "8", 7), ("0", "7", 8), ("1", "2", 8), ("3", "4", 9), ("5", "4", 10), ("1", "7", 11), ("3", "5", 14)]

gTest2 :: RoadMap
gTest2 = [("0", "1", 10), ("0", "2", 15), ("0", "3", 20), ("1", "2", 35), ("1", "3", 25), ("2", "3", 30)]

gTest3 :: RoadMap 
gTest3 = [("0", "1", 4), ("2", "3", 2)]
