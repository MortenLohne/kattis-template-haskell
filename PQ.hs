type Node = Int 

-- PRIORITY QUEUE
data SkewHeap a = Empty | SkewNode a (SkewHeap a) (SkewHeap a) deriving (Show)
type PriorityQueue = SkewHeap PQNode 
data PQNode = PQNode Node Int
instance Ord PQNode where 
    (PQNode _ dist1) `compare` (PQNode _ dist2) = dist1 `compare` dist2
instance Eq PQNode where 
    (PQNode _ dist1) == (PQNode _ dist2) = dist1 == dist2 


(+++) :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
heap1@(SkewNode x1 l1 r1) +++ heap2@(SkewNode x2 l2 r2) 
  | x1 <= x2    = SkewNode x1 (heap2 +++ r1) l1 
  | otherwise = SkewNode x2 (heap1 +++ r2) l2
Empty +++ heap = heap
heap +++ Empty = heap


delMin :: PriorityQueue -> Maybe (Node, PriorityQueue)
delMin Empty = Nothing
delMin (SkewNode (PQNode node _) l r ) = 
    Just (node, l +++ r )


addToPQ :: PriorityQueue -> Node -> Int -> PriorityQueue
addToPQ pq v dist = 
    let 
        node = (PQNode v dist)
        skewNode = SkewNode node Empty Empty 
    in 
        pq +++ skewNode 