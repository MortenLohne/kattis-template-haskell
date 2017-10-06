import Data.Array.IO

type Adj = [Edge]
type Node = Int 
type Weight = Int 
type Edge = (Node, Weight)

type Graph = IOArray Node Adj
type EdgeArray = IOArray Node Edge 
type DistArray = IOArray Node Int 
type VisitedArray = IOArray Node Bool 

dijkstra :: Graph -> PriorityQueue -> DistArray -> EdgeArray -> VisitedArray -> Node -> Node -> IO (DistArray, EdgeArray)
dijkstra graph pq distTo edgeTo visited source goal =
    case delMin pq of 
        Nothing -> return (distTo, edgeTo) -- PQ empty, we stop
        Just (node, pq') -> do
            writeArray visited node True 
            distToNode <- readArray distTo node 
            edgesOut <- readArray graph node

            if node == goal 
                then return (distTo, edgeTo) -- we are done, if we just wanted source to goal
            else do
                (pq'', distTo', edgeTo') <- relaxNeighbors pq' distTo edgeTo node distToNode visited edgesOut
                dijkstra graph pq'' distTo' edgeTo' visited source goal


relaxNeighbors :: PriorityQueue -> DistArray -> EdgeArray -> Node -> Int -> VisitedArray -> Adj -> IO (PriorityQueue, DistArray, EdgeArray)
relaxNeighbors pq distTo edgeTo _ _ _ [] = return (pq, distTo, edgeTo)
relaxNeighbors pq distTo edgeTo v distToV visited ((w,weight):adj) = do 
    isVisited <- readArray visited w
    if isVisited then 
        relaxNeighbors pq distTo edgeTo v distToV visited adj 
    else do 
        distToW <- readArray distTo w 
        if distToV + weight < distToW then do 
            writeArray distTo w (distToV + weight)
            writeArray edgeTo w (v, weight)
            let pq' = addToPQ pq w (distToV + weight)
            relaxNeighbors pq' distTo edgeTo v distToV visited adj 
        else 
            relaxNeighbors pq distTo edgeTo v distToV visited adj 
