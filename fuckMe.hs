type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

gTest1 :: RoadMap
gTest1 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest2 :: RoadMap -- unconnected graph
gTest2 = [("0","1",4),("2","3",2)]

--7

adjacent :: RoadMap -> City ->[(City,Distance)]
adjacent roadmap city = [(c2, d) | (c1, c2, d) <- roadmap, c1 == city] ++ [(c1, d) | (c1, c2, d) <- roadmap, c2 == city, c1 /= city]

--8 no fucking chance

areConnected :: RoadMap -> City -> City -> Bool
areConnected roadmap start end
  | start == end = True
  | otherwise = dfs [start] []
  where
    dfs [] _ = False
    dfs (current:stack) visited
      | current == end = True
      | current `elem` visited = dfs stack visited
      | otherwise = dfs (adjacentCities ++ stack) (current : visited)
      where
        adjacentCities = [c | (c, _) <- adjacent roadmap current]
