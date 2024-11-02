import Data.Array qualified
import Data.Bits qualified
import Data.List qualified

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

distance :: RoadMap -> City -> City -> Maybe Distance
distance roadmap city1 city2
  | areAdjacent roadmap city1 city2 =
      let (_, _, d) = head $ filter (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) roadmap
       in Just d
  | otherwise = Nothing

adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent roadmap city =
  [ if c1 == city then (c2, d) else (c1, d)
  | (c1, c2, d) <- roadmap,
    c1 == city || c2 == city
  ]

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance roadmap path = fmap sum (sequence distances)
  where
    distances = [distance roadmap city1 city2 | (city1, city2) <- zip path (tail path)]

rome :: RoadMap -> [City]
rome roadmap =
  let cities = [city | (city1, city2, _) <- roadmap, city <- [city1, city2]]
      grouped = Data.List.group $ Data.List.sort cities
      maxDegree = maximum (map length grouped)
   in [head group | group <- grouped, length group == maxDegree]

reachableFrom :: RoadMap -> City -> [City]
reachableFrom roadmap city = dfs [city] []
  where
    dfs [] visited = visited
    dfs (current : stack) visited
      | current `elem` visited = dfs stack visited
      | otherwise = dfs (adjacentCities ++ stack) (current : visited)
      where
        adjacentCities = [c | (c, _) <- adjacent roadmap current]

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadmap = all (\city -> length (reachableFrom roadmap city) == length (cities roadmap)) (cities roadmap)

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap start end
  | start == end = [[start]] -- The only shortest path from a city to itself
  | otherwise = bfs [[start]] -- Start with the initial path containing the start city
  where
    bfs [] = [] -- If no paths left to explore, return an empty list
    bfs paths =
      let nextPaths = concatMap extendPath paths -- Extend each path to find new paths
          validPaths = filter (not . null) nextPaths -- Remove invalid paths
          finishedPaths = filter ((== end) . head) validPaths -- Filter for completed paths
       in if null finishedPaths
            then bfs (filter (not . null) nextPaths) -- Continue searching if no finished paths
            else
              let minDistance = minimum (map pathDistance finishedPaths) -- Find minimum distance of valid paths
               in filter ((== minDistance) . pathDistance) finishedPaths -- Return only paths with the minimum distance
    extendPath path@(current : _) =
      [ newPath
      | (nextCity, _) <- adjacent roadmap current,
        let newPath = nextCity : path,
        nextCity `notElem` path -- Avoid cycles
      ]

    -- Calculate total distance for a given path
    pathDistance p =
      sum
        [ d
        | (c1, c2, d) <- roadmap,
          (head p == c1 && p !! 1 == c2)
            || (head p == c2 && p !! 1 == c1)
            || (last p == c1 && p !! (length p - 2) == c2)
            || (last p == c2 && p !! (length p - 2) == c1)
            || any (\(x, y) -> (x == c1 && y == c2) || (x == c2 && y == c1)) (zip p (tail p))
        ]

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7", "6", 1), ("8", "2", 2), ("6", "5", 2), ("0", "1", 4), ("2", "5", 4), ("8", "6", 6), ("2", "3", 7), ("7", "8", 7), ("0", "7", 8), ("1", "2", 8), ("3", "4", 9), ("5", "4", 10), ("1", "7", 11), ("3", "5", 14)]

gTest2 :: RoadMap
gTest2 = [("0", "1", 10), ("0", "2", 15), ("0", "3", 20), ("1", "2", 35), ("1", "3", 25), ("2", "3", 30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0", "1", 4), ("2", "3", 2)]