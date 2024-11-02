import qualified Data.Array
import qualified Data.Bits
import qualified Data.List

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String

type Path = [City]

type Distance = Int

type RoadMap = [(City, City, Distance)]

-- the function creates a list of unique cities from the given roadmap.
-- roadmap: a roadmap containing cities and distances between each pair of cities.
-- the function returns a list of unique cities that are in the roadmap.
cities :: RoadMap -> [City]
cities roadmap = Data.List.nub [city | (city1, city2, _) <- roadmap, city <- [city1, city2]]

-- the function checks if two cities are adjacent in the given roadmap.
-- roadmap: a roadmap containing cities and distances between each pair of cities.
-- city: the cities that we want to check if they are adjacent.
-- the function returns true if the cities are adjacent, otherwise false.
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadmap city1 city2 = any (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) roadmap

-- the function returns the distance between two cities only if they are adjacent.
-- roadmap: a roadmap containing cities and distances between each pair of cities.
-- city: the cities that we want to check if they are adjacent.
-- the function returns just the distance if the cities are adjacent, otherwise returns "Nothing".
distance :: RoadMap -> City -> City -> Maybe Distance
distance roadmap city1 city2 =
  case filter (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) roadmap of
    ((_, _, d) : _) -> Just d
    [] -> Nothing

-- the function shows a list of cities adjacent to the given city along with their distances.
-- roadmap: a roadmap containing cities and distances between each pair of cities.
-- city: the city that we want to find its adjacent cities.
-- the function returns a list of groups containing adjacent cities and their distances.
adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent roadmap city =
  [ if c1 == city then (c2, d) else (c1, d)
  | (c1, c2, d) <- roadmap,
    c1 == city || c2 == city
  ]

-- the function calculates the total distance of a given path.
-- roadmap: a roadmap containing cities and distances between each pair of cities.
-- path: the path for which to calculate the total distance.
-- the function returns just the total distance if the path is valid, otherwise Nothing.
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance roadmap path = fmap sum (sequence distances)
  where
    distances = [distance roadmap city1 city2 | (city1, city2) <- zip path (tail path)]


-- | Finds the city with the highest degree (most connections) in the roadmap.
-- 
-- rome :: RoadMap -> [City]
-- @param roadmap The roadmap containing cities and distances.
-- @return A list of cities with the highest degree.
rome :: RoadMap -> [City]
rome roadmap =
  let cityDegrees = [(city, length [(c1, c2, d) | (c1, c2, d) <- roadmap, city == c1 || city == c2]) | city <- cities roadmap]
      maxDegree = maximum (map snd cityDegrees)
  in [city | (city, degree) <- cityDegrees, degree == maxDegree]

-- | Finds all cities reachable from the given city.
-- 
-- reachableFrom :: RoadMap -> City -> [City]
-- @param roadmap The roadmap containing cities and distances.
-- @param city The starting city.
-- @return A list of cities reachable from the starting city.
reachableFrom :: RoadMap -> City -> [City]
reachableFrom roadmap city = dfs [city] []
  where
  dfs [] visited = visited
  dfs (current : stack) visited
    | current elem visited = dfs stack visited
    | otherwise = dfs (adjacentCities ++ stack) (current : visited)
    where
    adjacentCities = [c | (c, _) <- adjacent roadmap current]

-- | Checks if the roadmap is strongly connected.
-- 
-- isStronglyConnected :: RoadMap -> Bool
-- @param roadmap The roadmap containing cities and distances.
-- @return True if the roadmap is strongly connected, otherwise False.
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadmap = all (\city -> length (reachableFrom roadmap city) == length (cities roadmap)) (cities roadmap)

-- | Finds the shortest path between two cities.
-- 
-- shortestPath :: RoadMap -> City -> City -> [Path]
-- @param roadmap The roadmap containing cities and distances.
-- @param start The starting city.
-- @param end The destination city.
-- @return A list of paths representing the shortest path(s) between the two cities.
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap start end
    | start == end = [[start]]
    | otherwise = filter ((== minDist) . pathDist) allPaths
    where
      allPaths = dfs [[start]]
      dfs [] = []
      dfs (path:paths)
        | otherwise = dfs (paths ++ [path ++ [next] | (next, _) <- adjacent roadmap (last path), next notElem path])
        | otherwise = dfs (paths ++ [path ++ [next] | (next, _) <- adjacent roadmap (last path), next notElem path])
      pathDist path = case pathDistance roadmap path of
        Just d -> d
        Nothing -> maxBound
      minDist = minimum (map pathDist allPaths)

-- | Solves the Traveling Salesman Problem using a brute force approach.
-- 
-- travelSales :: RoadMap -> Path
-- @param roadmap The roadmap containing cities and distances.
-- @return The shortest path that visits all cities exactly once and returns to the starting city.
travelSales :: RoadMap -> Path
travelSales roadmap
  | null citiesList = []
  | otherwise = snd $ minimum [(fromJust (pathDistance roadmap path), path) | path <- allPaths]
  where
    citiesList = cities roadmap
    allPaths = [start : path ++ [start] | start <- citiesList, path <- permutations (filter (/= start) citiesList)]
    permutations [] = [[]]
    permutations xs = [x : ps | x <- xs, ps <- permutations (filter (/= x) xs)]
    fromJust (Just x) = x
    fromJust Nothing = error "Unexpected Nothing"

tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7", "6", 1), ("8", "2", 2), ("6", "5", 2), ("0", "1", 4), ("2", "5", 4), ("8", "6", 6), ("2", "3", 7), ("7", "8", 7), ("0", "7", 8), ("1", "2", 8), ("3", "4", 9), ("5", "4", 10), ("1", "7", 11), ("3", "5", 14)]

gTest2 :: RoadMap
gTest2 = [("0", "1", 10), ("0", "2", 15), ("0", "3", 20), ("1", "2", 35), ("1", "3", 25), ("2", "3", 30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0", "1", 4), ("2", "3", 2)]