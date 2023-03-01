module Cluster where
import Data.List
import Data.Maybe
import Debug.Trace
import Distribution.Simple.Utils (xargs)
import System.Directory.Internal.Prelude (on)

--Points are an x coordinate, a y coordinate, and an integer label.
type Point = (Double, Double, Int)
type Center = Point
--A cluster is a center and a list of points.
type Cluster = (Center, [Point])

-- All undefined values and functions should be completed. Your code will compile and test 
-- -- (with the -- test flag) even if some functions are left undefined.
--
-- --                                       Milestone
--

--Given a list of elements and an integer k, return k evenly spaced elements of the list.
--As a first attempt, simply take the first k elements. Fix this after you finish the rest of the
--project.
getKElems :: Int -> [a] -> [a]
getKElems int lst = take int lst
    --let a = (fromIntegral(length lst - 1) :: Float) / (fromIntegral(int - 1) :: Float)
        --b = take int lst
    --in map a b

--Example: getKElems 3 [1..6] = [1,3,6]

--Return the Euclidean distance between two points. You may work in squared distances if you
--prefer.
eucDist :: Point -> Point -> Double
eucDist (x1, y1, label1) (x2, y2, label2) = sqrt ( (x2 - x1)^2 + (y2 - y1)^2)
--Example: eucDist (0,0,10) (1,1,10) < eucDist (0,0,10) (0,2,10)

--Return the Manhattan distance between two points: the distance between the points based on
--strictly horizontal and vertical movement, as if on a city grid.
manhatDist :: Point -> Point -> Double
manhatDist (x1, y1, label1) (x2, y2, label2) = abs(x2 - x1) + abs(y2 - y1)
--Example: manhatDist (0,0,10) (1,1,10) == manhatDist (0,0,10) (0,2, 10)

--Return the Chebyshev distance between two points: the maximum between the x-distance and the
--y-distance, as if diagonal movement was free, like for a King in chess.
chebyDist :: Point -> Point -> Double
chebyDist (x1, y1, label1) (x2, y2, label2) = max(abs(x2 - x1)) (abs(y2 - y1))
--Example: chebyDist (0,0,10) (0,5,10) == chebyDist (0,0,10) (5,5,10)

--Return the traffic-aware Manhattan distance: count horizontal distances twice as much as vertical.
trafficDist :: Point -> Point -> Double
trafficDist (x1, y1, label1) (x2, y2, label2) = 2*(abs(x2 - x1)) + abs(y2 - y1)
--Example: trafficDist (0,0,10) (0,10,10) == trafficDist (0,0,10) (5,0,10)

--Return the township-aware Manhattan distance. The label of the point is taken to be the township
--the point is in.  A penalty factor of 2 is applied to the distance between points in different
--townships.
townshipDist :: Point -> Point -> Double
townshipDist (x1, y1, label1) (x2, y2, label2) = if label1 /= label2 then 2*dist else dist
    where dist = manhatDist (x1, y1, label1) (x2, y2, label2)
--Example: townshipDist (0,0,10) (1,1,20) == 2*townshipDist (0,0,10) (1,1,10) 

--Given a list of doubles, compute their average. You may need fromIntegral.
average :: [Double] -> Double
average list = sum list/ fromIntegral(length list)
--Example:  average [0,5,10] = 5.0

--Given a ranking function and a list of elements, return the element with the minimum rank.
minimize :: (a -> Double) -> [a] -> a
minimize f (x:xs) = aux xs (f x) x
    where aux [ ] minimum least = least
          aux (x:xs) minimum least  
            | curr < minimum = aux xs curr x
            | otherwise = aux xs minimum least
                where curr = f x

--Example: minimize (fromIntegral . length) ["aaaa", "b"] = "b"

--Given a bucketing function, a list of buckets, and a list of items to be placed in buckets, 
--and returns the list of bucket, items-in-buckets pairs.
--Go take your old buildCorpus function, copy it, and turn it into a HOF (higher-order function).
-- Bucket is a HOF, because it takes a function as input. You can write it using recursion, other
-- HOFs, or list comprehensions as you choose.
bucket :: Eq b => (a -> b) -> [b] -> [a] -> [(b,[a])]
bucket f b a = [(x, buckett x) | x <- b] 
    where buckett x = filter (\k -> (f k) == x) a
--Example:  bucket length [1..3] ["Hi","my","job","is","fun","!"]
--[(1,["!"]),(2,["Hi","my","is"]),(3,["job","fun"])]
--

--Full project!

--Given a metric, a list of centers, and a point, return the center closest to the point.
--Hint: you've already written a higher-order function to do this.
assignPoint :: (Point -> Center -> Double) -> [Center] -> Point -> Center
assignPoint m c p = minimize (m  p) c 
--Examples: assignPoint eucDist  [(0,0,-1),(5,7,-1)] (5,0,100) = (0.0,0.0,-1)
--          assignPoint trafficDist  [(0,0,-1),(5,7,-1)] (5,0,100) = (5.0,7.0,-1)

--Given a metric, a list of centers, and a list of point, return the clusters, where each point is
--assigned to the nearest center.
--Hint: you've already written a higher-order function to do this.
assignPoints :: (Point -> Center -> Double) -> [Center] -> [Point] -> [Cluster]
assignPoints f c p = bucket (assignPoint f c) c p
--Examples:
--let testClusters = assignPoints trafficDist [(1,1,-1),(2,2,-1)] tenPoints 
--
--[(c, length ps) | (c,ps) <- testClusters]
--[((1.0,1.0,-1),1),((2.0,2.0,-1),9)]
--
--testClusters
--[((1.0,1.0,-1),[(1.0,7.0,700)]),
-- ((2.0,2.0,-1),[(7.0,1.0,100),(7.0,3.0,200),(8.0,1.0,300),(8.0,2.0,400),(7.5,3.5,500),
--                (2.0,7.0,600),(3.0,7.0,800),(2.0,8.0,900),(2.0,6.0,1000)])]


--Given a metric and a cluster, return the mean of the points in the cluster.
--The existing center is NOT a point, and should be ignored.
--The label should be the label of the closest point to the new center. 
--Since you can't take the mean of an empty list of points, return Nothing in that case.
findMean :: (Point -> Center -> Double) -> Cluster -> Maybe Center
findMean f (cent, [ ]) = Nothing 
findMean f clust = 
    let (cent, point) = clust
        newX = average [ x | (x, y, z) <- point] 
        newY = average [ y | (x, y, z) <- point] 
        label = 1 + maximum [ z | (x, y, z) <- point] 
        (x, y, z) = assignPoint f point (newX, newY, label)
    in Just (newX, newY, z)
        
    --let c = fst clust 
        --pList = snd clust
        --a = [x | x <- pList, fst x]
    --in average pLists
--Example: findMean eucDist ((3,3,0), [(0,0,0), (10,10,0), (2,2,1)]) = Just (4.0,4.0,1)

--Given a metric and a list of clusters, relocate all the centers to the mean of their clusters. Be
--sure to return only the valid centers. If any cluster is empty, simply remove it.
moveCenters :: (Point -> Center -> Double) -> [Cluster] -> [Center]
moveCenters m clust = 
    let validClusters = [ x | x <- clust, snd x /= [ ]]
    in [ fromMaybe (0,0,0) (findMean m x) | x <- validClusters]

    --Map.foldWithKey insertCenter Map.empty centroidsMap
    --where insertCenter _ ps m = Map.insert (center ps) ps m
        --center [ ] = [0, 0]
        --center ps = map average (transpose ps)
        --average xs = sum xs / fromIntegral (length xs)
--Example:  moveCenters trafficDist testClusters  = [(1.0,7.0,700),(5.166666666666667,4.277777777777778,200)]

--Given a metric, k, and a list of clusters, first move the centers, and then reassign the points
--to the new centers.
--Note that clusters can become empty and disappear. For full credit, replace missing clusters as
--described on the website.

splitCluster :: Int -> [Center] -> [Cluster] -> [Center]
splitCluster k c clst = 
    let points = [snd x | x <- clst] 
        bigLst = maximumBy (compare `on` length) points 
        bigCent = fst(head(filter(\(x,y) -> y == bigLst) clst ))
        rVar = [ x | x <- bigLst, x /= bigCent]
    in if (length rVar) > 1 then c ++ [head rVar] else c 

improveClusters :: (Point -> Center -> Double) -> Int -> [Cluster] -> [Cluster]
improveClusters m k clust = 
    let pointList = concat [ snd x | x <- clust]
        centers = moveCenters m clust 
        newCenters = if length centers == k then centers else splitCluster k centers clust
    in  assignPoints m newCenters pointList

--Example: let newClusters = improveClusters trafficDist 2 testClusters 
--[(c, length ps) | (c,ps) <- newClusters]
--[((1.0,7.0,700),5),((5.166666666666667,4.277777777777778,200),5)]

--iterationLimit should be used by kMeans to limit how many times improveClusters can be called.
--Remember variables are not mutable.
iterationLimit = 100
--Given a metric, k, and a list of points, create the initial clusters and repeatedly 
--improve them, until they stop changing.
kMeans :: (Point -> Center -> Double) -> Int -> [Point] -> [Cluster]
kMeans m k pntLst = 
    let centers =  getKElems k pntLst 
        clusters = assignPoints m centers pntLst 
        improvements = improveClusters m k clusters 
    in helperK m k clusters 

helperK :: (Point -> Center -> Double) -> Int -> [Cluster] -> [Cluster]
helperK m k clstLst = 
    let improvements = improveClusters m k clstLst 
    in if clstLst == improvements then clstLst else helperK m k improvements 


