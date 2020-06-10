module Graph(Graph(..),Edge(..),vertices,emptyGraph,insert,lookupGraph)
where

--  import qualified Data.Set as Set
import  Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

data Edge a b = Edge{
    from::a
    ,to::a,
    cost::b
} deriving (Show,Eq,Ord)


-- fillMap :: [LineTable] -> Graph String Integer -> Graph String Integer



data Graph a b = Graph{
    adjMap :: Map a [Edge a b]
} deriving Show


vertices :: Graph a b -> [a]
vertices = M.keys . adjMap

emptyGraph :: Graph a b
emptyGraph = Graph M.empty

insert :: Ord a => a -> Edge a b -> Graph a b->Graph a b
insert key edge (Graph adjMap ) 
    | isNothing((M.lookup key adjMap))  =  Graph (M.insert key [edge] adjMap)
    | otherwise =  Graph (M.insert key (edge: fromJust (M.lookup key adjMap)) adjMap) 


lookupGraph:: Ord a => a -> Graph a b -> Maybe [Edge a b] 
lookupGraph k (Graph l) = M.lookup k l     



--edge = Edge "from1" "to1" 100


