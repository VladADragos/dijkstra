import Route
import RouteGUI
import Graph  -- Create a module and use a sensible graph representation
import Data.Maybe
import qualified Data.List as L
import qualified Data.OrdPSQ as PQ
import Data.Map(Map)
import qualified Data.Map as M
import System.IO
import System.Environment

-- types
type Set = Map String (String,Integer)
type Edge' = Edge String Integer 

type PQ' = PQ.OrdPSQ Edge' Integer Edge'
type Graph'= Graph String Integer
type Edges = [Edge']
                              
getPath:: Name -> Set-> ([Name], Integer)
getPath to s
  | weight == 0 = ([to], 0)
  | otherwise = ( [to]++nodes, weight)
    where (nodes, _) = getPath from s
          (from, weight) = fromJust $ M.lookup to s  

        
shortestPath :: Graph' -> Name -> Name -> Maybe ([Name], Integer)
shortestPath g from to
        |nodeExistsInGraph from || nodeExistsInGraph to =Nothing
        |otherwise= Just ( reverse nodes, cost)
         where
           (nodes,cost) =  getPath to $ shortestPath' g M.empty $ PQ.singleton firstEdge' 0 firstEdge'
           firstEdge' = (Edge from from 0)
           vertices' = vertices g 
           nodeExistsInGraph node = not $ elem node vertices'


shortestPathAll :: Graph' -> Name -> Name -> Maybe (Set)
shortestPathAll g from to
        |nodeExistsInGraph from || nodeExistsInGraph to =Nothing
        |otherwise= Just $ shortestPath' g M.empty $ PQ.singleton firstEdge' 0 firstEdge'
         where
           firstEdge' = (Edge from from 0)
           vertices' = vertices g 
           nodeExistsInGraph node = not $ elem node vertices'
        
shortestPath' :: Graph' -> Set -> PQ' -> Set
shortestPath' g s pq 
    | not (PQ.null pq) = shortestPath' g s' pq'
    | otherwise  = s
    where (s',pq') = shortestPath'' g pq s
    

shortestPath'' :: Graph'-> PQ' -> Set -> (Set, PQ')
shortestPath'' g pq s  
        | (M.notMember to s )   = ( M.insert to (from, weight) s, addNeighboursToPQ neighbours weight (PQ.deleteMin pq) s)
        | otherwise = (s,addNeighboursToPQ neighbours weight (PQ.deleteMin pq) s)
        where (edge@(Edge from to _), weight,_) =  fromJust $ PQ.findMin pq
              neighbours = fromJust $ lookupGraph to g 
              

addNeighboursToPQ:: [Edge'] -> Integer -> PQ' -> Set -> PQ' 
addNeighboursToPQ [] _ pq _= pq
addNeighboursToPQ (edge@(Edge from to weight):xs) prevWeight pq s 
    | (M.notMember to s)  = addNeighboursToPQ xs prevWeight (PQ.insert k p v pq) s
    | otherwise = addNeighboursToPQ xs prevWeight pq s
    where (k,p,v) = (edgeToKPV (Edge from to cost))
          (_,inSetWeight) = fromJust $ M.lookup to s
          cost = prevWeight + weight     


filter' :: Edges->Edges
filter' = reverse . L.nub . reverse


edgeToKPV :: Edge' -> (Edge',Integer ,Edge') 
edgeToKPV edge@(Edge from to weight) = (edge,weight,edge)

testMain :: IO ()
testMain = do 
  Right stops <- readStops "./text-files/stops-air-smal.txt"
  Right lines <- readLines "./text-files/lines-air-smal.txt"
  let graph = populateMap lines emptyGraph
  let _shortestPath = fromJust $ shortestPath graph "A" "E"
  putStrLn $ show $ _shortestPath
  return ()

main :: IO ()
main = do 
  [stopsFile, linesFile, from, to] <- getArgs
  Right stops <- readStops stopsFile
  Right lines <- readLines linesFile
  let graph = populateMap lines emptyGraph
  let _shortestPath = fromJust $ shortestPath graph from to
  putStrLn $ show $ _shortestPath
  return ()



populateMap ::  [LineTable] -> Graph'  -> Graph'
populateMap [] g = g
populateMap ((LineTable _ stops):lts) g = populateMap lts $ populateMap'' stops g

populateMap'' ::[LineStop] ->Graph' -> Graph'
populateMap'' [] g = g
populateMap'' (s1:s2:[]) g =  populateMap' (s1,s2) g
populateMap'' (s1:s2:ss) g =  populateMap'' (s2:ss) $ populateMap' (s1,s2) g
populateMap'' (s1:[]) g    = error "You're not supposed to be able to get here though"

populateMap' :: (LineStop,LineStop) -> Graph' -> Graph'
populateMap' ((LineStop n1 c1),(LineStop n2 c2)) g = insert n1 (Edge n1 n2 c2) g

startGUI :: IO ()
startGUI = do
  Right stops <- readStops "text-files/stops-air.txt"
  Right lines <- readLines "text-files/lines-air.txt"
  let graph = populateMap lines emptyGraph
  runGUI stops lines graph shortestPath