import qualified Data.List
-- import qualified Data.Array
-- import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String

type Path = [City]

type Distance = Int

type RoadMap = [(City, City, Distance)]

-- The function creates a list of unique cities from the given roadmap.
-- Arguments: roadmap - a list of tuples with the cities and the distance between each pair of cities.
-- The function returns the list of unique cities
cities :: RoadMap -> [City]
cities roadmap = Data.List.nub [city | (city1, city2, _) <- roadmap, city <- [city1, city2]]

-- The function checks if two cities are adjacent in the given roadmap.
-- Arguments: roadmap- the representation of the graph, and the two cities that need to be checked
-- The function returns true if the cities are adjacent and false otherwise.
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadmap city1 city2 = any (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) roadmap

-- The function returns the distance between two adjiacent cities.
-- Arguments: roadmap- the list of cities and distances, and the two cities we want to check
-- The function returns "Just [distance]" if the cities are adjacent, and "Nothing" otherwise.
distance :: RoadMap -> City -> City -> Maybe Distance
distance roadmap city1 city2 =
  case filter (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) roadmap of
    ((_, _, d) : _) -> Just d
    [] -> Nothing

-- The function displays a list of cities adjacent to the given city along with their distances.
-- Arguments: the roadmap and the city for which the adjiacent cities are needed
-- The function returns a list of tuples containing the adjacent cities and their distances.
adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent roadmap city =
  [ if c1 == city then (c2, d) else (c1, d)
  | (c1, c2, d) <- roadmap,
    c1 == city || c2 == city
  ]

-- The function calculates the total distance of a given path.
-- Arguments: the roadmap, and the path- a list of city values for which the total distance is needed
-- For a valid path the function returns the total distance, and "Nothing" otherwise.
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance roadmap path = fmap sum (sequence distances)
  where
    distances = [distance roadmap city1 city2 | (city1, city2) <- zip path (tail path)]

-- The function finds the city with the most connecting roads in the roadmap
-- Arguments: roadmap- the list of tuples with the cities and their distances
-- The function returns a list of cities with the highest number of connecting roads
rome :: RoadMap -> [City]
rome roadmap =
  let cityDegrees = [(city, length [(c1, c2, d) | (c1, c2, d) <- roadmap, city == c1 || city == c2]) | city <- cities roadmap]
      maxDegree = maximum (map snd cityDegrees)
   in [city | (city, degree) <- cityDegrees, degree == maxDegree]

-- The function checks if the roadmap is strongly connected (any city can be reached from any other city)
-- Arguments: the roadmap with the cities and distances
-- Returns True if the roadmap is strongly connected and False otherwise.
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadmap = all (\city -> length (dfs [city] []) == length (cities roadmap)) (cities roadmap)
  where
    dfs [] visited = visited
    dfs (current : stack) visited
      | current `elem` visited = dfs stack visited
      | otherwise = dfs (adjacentCities ++ stack) (current : visited)
      where
        adjacentCities = [c | (c, _) <- adjacent roadmap current]

-- The function finds the shortest path between two cities.
-- Arguments: the roadmap, city1- the starting city, city2- the ending city
-- Returns alist of paths representing the shortest path(s) between the two cities or an empty list if there is no path.
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap start end
  | start == end = [[start]]
  | otherwise = filter ((== minDist) . pathDist) allPaths
  where
    allPaths = dfs [[start]]
    dfs [] = []
    dfs (path : paths)
      | last path == end = path : dfs paths
      | otherwise = dfs (paths ++ [path ++ [next] | (next, _) <- adjacent roadmap (last path), next `notElem` path])
    pathDist path = case pathDistance roadmap path of
      Just d -> d
      Nothing -> maxBound
    minDist = minimum (map pathDist allPaths)

-- The function solves the Traveling Salesman Problem(the shortest route that visits every city only once and it returns to the same starting point)
-- Arguments: roadmap- the roadmap containing cities and distances.
-- The function returns shortest path that satisfies the conditions or an empty list if there is no path found.
travelSales :: RoadMap -> Path
travelSales roadmap
  | null citiesList = []
  | null validPaths = []
  | otherwise = snd $ minimum validPaths
  where
    citiesList = cities roadmap
    allPaths = [start : path ++ [start] | start <- citiesList, path <- permutations (filter (/= start) citiesList)]
    validPaths = [(d, path) | path <- allPaths, let d = pathDistance roadmap path, d /= Nothing]
    permutations [] = [[]]
    permutations xs = [x : ps | x <- xs, ps <- permutations (filter (/= x) xs)]

tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7", "6", 1), ("8", "2", 2), ("6", "5", 2), ("0", "1", 4), ("2", "5", 4), ("8", "6", 6), ("2", "3", 7), ("7", "8", 7), ("0", "7", 8), ("1", "2", 8), ("3", "4", 9), ("5", "4", 10), ("1", "7", 11), ("3", "5", 14)]

gTest2 :: RoadMap
gTest2 = [("0", "1", 10), ("0", "2", 15), ("0", "3", 20), ("1", "2", 35), ("1", "3", 25), ("2", "3", 30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0", "1", 4), ("2", "3", 2)]