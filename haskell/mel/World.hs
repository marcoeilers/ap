module World
       ( Direction(..)
       , Position
       , Cell
       , Maze
       , Robot(..)
       , World
       , validMove
       , fromList
       , neighbor
       , getGoalPos
       , getCell
       , turnRight
       , turnLeft
       ) where

import qualified Data.Map as M
import qualified Data.List as L

data Direction = North | South | West | East deriving (Show, Eq, Read)

turnRight :: Direction -> Direction
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

turnLeft :: Direction -> Direction
turnLeft North  = West
turnLeft West   = South
turnLeft South  = East
turnLeft East   = North

type Position = (Int, Int)
type Cell = [Direction]

-- | The Maze data type
data Maze = Maze { width  :: Int
                 , height :: Int
                 , cells  :: M.Map Position Cell
                 } deriving (Show)

data Robot = Robot { pos  :: Position
                   , dir  :: Direction
                   , hist :: [Position]
                   } deriving (Show, Eq)

type World = (Maze, Robot)

getGoalPos :: Maze -> Position
getGoalPos maze = (width maze - 1, height maze - 1)

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
validMove maze p q = case mcell of (Just cell) -> (getDirection p q) `notElem` cell
                                   Nothing     -> False
  where mcell = M.lookup p (cells maze)

fromList :: [(Position, Cell)] -> Maze
fromList cells = Maze width height m
  where m = M.fromList cells
        (width,height) = check m

check' cells = (w+1, h+1)
  where (w,h) = last sorted
        sorted = L.sort $ M.keys cells

{- The following section is sort of ruled out. -}
        
-- | We require that ALL positions in the maze have been specified, 
-- hence we throw an error if this is not the case. Additionally,
-- the maze must be completely surrounded by walls, and neighboring
-- cells must have compatible walls. If all conditions are astisfied, 
-- we return the height and width of the maze in a tuple.
check :: M.Map Position Cell -> (Int, Int)
check cells = if and [allThere
                     , westCorrect
                     , eastCorrect
                     , northCorrect
                     , southCorrect
                     , neighborsEW
                     , neighborsNS ]
              then (w+1, h+1)
              else error "Maze not wellformed"
  where poss         = M.keys cells
        sorted       = L.sort poss
        (w,h)        = last sorted
        allThere     = sorted == L.sort [ (w',h') | w' <- [0..w], h' <- [0..h] ]
        westCorrect  = checkBorders (Just 0) Nothing West poss cells
        eastCorrect  = checkBorders (Just w) Nothing East poss cells
        northCorrect = checkBorders Nothing (Just h) North poss cells
        southCorrect = checkBorders Nothing (Just 0) South poss cells
        neighborsEW  = all (checkNeighbors 1 0 East West cells) [ (w',h') | w' <- [0..w-1], h' <- [0..h] ]
        neighborsNS  = all (checkNeighbors 0 1 North South cells) [ (w',h') | w' <- [0..w], h' <- [0..h-1] ]


checkNeighbors :: Int -> Int -> Direction -> Direction -> M.Map Position Cell -> Position -> Bool
checkNeighbors dx dy dir1 dir2 cells pos@(x,y) = (dir1 `elem` (cells M.! pos )) 
                                                == (dir2 `elem` (cells M.! (x+dx, y+dy)))
        
checkBorders :: Maybe Int -> Maybe Int -> Direction -> [Position] -> M.Map Position Cell -> Bool
checkBorders mx my dir poss cells = xcorrect && ycorrect  
  where xcorrect = case mx of Nothing -> True
                              Just xc  ->  all (elem dir) [(cells M.! (x,y)) | (x, y) <- poss, xc==x ]
        ycorrect = case my of Nothing -> True                      
                              Just yc  -> all (elem dir) [(cells M.! (x,y)) | (x, y) <- poss, yc==y ]

{- OLEKS -1: This whole function is impossible to read. Keep your lines under
80 characters in width, so use line breaks more gratiously. -}

{- OLEKS -2: Don't throw error. Your runProg should be failing appropriately
instead if you insist on checking your assumptions (which is a bit redundant
anyways). -}

{- OLEKS -2: Again, M.! is a partial function, don't use partial functions. -}

-- | Get the direction to go to reach q from p
getDirection :: Position -> Position -> Direction
getDirection (pw,ph) (qw,qh) = case (qw-pw,qh-ph) of (0,1)  -> North
                                                     (0,-1) -> South
                                                     (1,0)  -> East
                                                     (-1,0) -> West

getCell :: Maze -> Position -> Maybe Cell
getCell maze pos = M.lookup pos $ cells maze

