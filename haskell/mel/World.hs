module World
       ( Direction(..)
       , Position
       , Cell
       , Maze
       , validMove
       , fromList
       , neighbor
       , getGoalPos
       , getCell
       , testMaze
       ) where

import qualified Data.Map as M
import qualified Data.List as L

data Direction = North | South | West | East deriving (Show, Eq, Read)

-- | arrange it so succ is the same as going right (from North -> East)
instance Enum Direction where
  toEnum n = [North, East, South, West] !! (n `mod` 4)
  
  fromEnum North = 0
  fromEnum East  = 1
  fromEnum South = 2
  fromEnum West  = 3



type Position = (Int, Int)
type Cell = [Direction]

-- | The Maze data type
data Maze = Maze { width  :: Int
                 , height :: Int
                 , cells  :: M.Map Position Cell
                 } deriving (Show)

getGoalPos :: Maze -> Position
getGoalPos maze = (width maze, height maze)

-- | Get the position from a given position and a direction
-- For instance neighbor (0,1) North #=> (0,2). This function 
-- performs no checks to see if we're within the maze (we don't have 
-- access to the maze here.
neighbor :: Position -> Direction -> Position
neighbor (w, h) d = case d of North -> (w,h+1)
                              South -> (w,h-1)
                              East  -> (w+1,h)
                              West  -> (w-1,h)

getNeighborCells :: Maze -> Position -> [(Position, Cell)]
getNeighborCells maze pos = [ (p,c) | (p, Just c) <- ns ]
  where ns = zip ps $ map lookup ps
        lookup = (flip M.lookup) (cells maze)
        ps = getNeighbors maze pos


getNeighbors :: Maze -> Position -> [Position]
getNeighbors maze (w,h) = filter withInMaze [(w-1,h), (w+1,h), (w,h-1), (w,h+1)]
  where withInMaze (w, h) = and [ w >= 0
                                , h >= 0
                                , w <= (width maze)
                                , h <= (height maze)]

-- | Assuming two cells are neigboring cells
validMove :: Maze -> Position -> Position -> Bool
validMove maze p q = (getDirection p q) `notElem` ((cells maze) M.! p)

fromList :: [(Position, Cell)] -> Maze
fromList cells = Maze width height $ M.fromList cells
  where (width,height) = check $ map fst cells

-- | We require that ALL positions in the maze have been specified, 
-- hence we throw an error if this is not the case. Otherwise we 
-- return the height and width of the maze in a tuple.
check :: [Position] -> (Int, Int)
check poss = if sorted == L.sort [ (w',h') | w' <- [0..w], h' <- [0..h] ]
             then (w, h)
             else error "Maze not welformed"
  where sorted = L.sort poss
        (w,h) = last sorted


-- | Get the direction to go to reach q from p
getDirection :: Position -> Position -> Direction
getDirection (pw,ph) (qw,qh) = case (qw-pw,qh-ph) of (0,1)  -> North
                                                     (0,-1) -> South
                                                     (1,0)  -> East
                                                     (-1,0) -> West

getCell :: Maze -> Position -> Cell
getCell maze pos = (cells maze) M.! pos

testMaze :: Maze
testMaze = fromList testCells

testCells :: [(Position, [Direction])]
testCells = [ ((0,0),[North,South,West])
            , ((0,1),[North,South,West])
            , ((0,2),[South,West])
            , ((0,3),[West,East])
            , ((0,4),[North,West])
            , ((1,0),[South])
            , ((1,1),[North])
            , ((1,2),[South,East])
            , ((1,3),[North,West])
            , ((1,4),[North,South,East])
            , ((2,0),[North,South])
            , ((2,1),[South,East])
            , ((2,2),[West,East])
            , ((2,3),[])
            , ((2,4),[North,West,East])
            , ((3,0),[North,South])
            , ((3,1),[South,West])
            , ((3,2),[West])
            , ((3,3),[])
            , ((3,4),[North,West,East])
            , ((4,0),[North,South,East])
            , ((4,1),[North,South,East])
            , ((4,2),[North,South,East])
            , ((4,3),[South,East])
            , ((4,4),[North,West,East])
            ]