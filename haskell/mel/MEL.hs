module MEL
       ( Relative(..)
       , Cond(..)
       , Stm(..)
       , Program
       , Result(..)
       , runProg
       , RobotCommand(..)
       , interp
       , evalCond
       , initialWorld )
       where

import World

-- | MEL interpreter data types
data Relative = Ahead | ToLeft | ToRight | Behind deriving (Eq, Show)

-- | Notes: The condition AtGoalPos is true if the robot is in the 
-- top right corner of the maze, otherwise it is false
data Cond = Wall Relative
          | And Cond Cond
          | Not Cond
          | AtGoalPos
          | Visited Position
          deriving (Eq, Show)

data Stm = Forward
         | Backward
         | TurnRight
         | TurnLeft
         | If Cond Stm Stm
         | While Cond Stm
         | Block [Stm]
         deriving (Eq, Show)

type Program = Stm

getRelDir :: Relative -> Direction -> Direction
getRelDir rel dir = case rel of Ahead   -> dir
                                ToLeft  -> turnLeft dir
                                ToRight -> turnRight dir
                                Behind  -> (turnRight . turnRight) dir

initialWorld :: Maze -> World
initialWorld maze = (maze, initialRobot)

-- | A new Robot in the lower left corner looking North
--   Should always be used when constructing new robots.
initialRobot :: Robot
initialRobot = Robot (0,0) North []


-- | The runRC function consumes a world and produces 
newtype RobotCommand a = RC { runRC :: World -> Either Robot (a, Robot) }

{- OLEKS 0: So why don't you just write runRC :: World -> .. ? It's a bit
nonobvious that if you get a Left Robot, then something went wrong. It would've
been nice with an error type to go along with the robot, indicating which error
occured. -}

-- | Our central monad
instance Monad RobotCommand where
  return a     = RC $ \(_,r) -> Right (a, r)
  (RC h) >>= f = RC $ \w@(m,r) -> do (a, r') <- h w
                                     let (RC g) = f a
                                     g (m, r')

getWorld :: RobotCommand World
getWorld = RC $ \world@(maze, robot) -> Right (world, robot)

putRobot :: Robot -> RobotCommand () 
putRobot robot = RC $ \_ -> Right ((), robot)

putError :: Robot -> RobotCommand ()
putError robot = RC $ \_ -> Left robot

-- | Interprets statements into robotCommands.
-- RobotCommand can then be applied to a world to get a result.
interp :: Stm -> RobotCommand ()
interp TurnRight = do
  (_,r) <- getWorld
  putRobot $ Robot (pos r) (turnRight (dir r)) (hist r)
interp TurnLeft = do
  (_,r) <- getWorld 
  putRobot $ Robot (pos r) (turnLeft (dir r)) (hist r)
interp Forward = do
  (m,r) <- getWorld
  let d = dir r
      p = pos r
      np = neighbor p d
  if validMove m p np
    then putRobot $ Robot np d (p : hist r)
    else putError r
interp Backward = do 
  (m,r) <- getWorld
  let d = dir r
      p = pos r
      np = neighbor p $ (turnRight . turnRight) d
  if validMove m p np
     then putRobot $ Robot np d (p : hist r)
     else putError r
interp (If cond true false) = do
  w <- getWorld
  interp $ if evalCond w cond 
           then true
           else false
interp (While cond stm) = do
  w <- getWorld
  if evalCond w cond
    then interp $ Block [stm, (While cond stm)] 
    else return ()
interp (Block [])         = return ()
interp (Block (stm:stms)) = do interp stm
                               interp $ Block stms

{- OLEKS -2: This is a little dirty. Please use do notation instead. Consider
defining getRobot :: RobotCommand Robot and putRobot :: Robot -> RobotCommand
(). -}

-- | Evaluates all conditions
evalCond :: World -> Cond -> Bool
evalCond w@(m,r) cond = case cond of
  (Wall rel)  -> (getRelDir rel (dir r)) `elem` (getCell m (pos r))
  (And c1 c2) -> (evalCond w c1) && (evalCond w c2)
  (Not c)     -> not (evalCond w c)
  AtGoalPos   -> (pos r) == getGoalPos m
  Visited pos -> pos `elem` (hist r)


data Result a = Success a | Failure a deriving (Show, Eq)

-- | runProg should ALWAYS be used to execute a program 
-- (outside of whitebox testing)
runProg :: Maze -> Program -> Result ([Position], Direction)
runProg maze program = let (RC prog) = interp program
                       in case (prog $ initialWorld maze) of
                         (Right (_, r)) -> Success $ status r
                         (Left r)       -> Failure $ status r
  where status robot = ((pos robot):(hist robot), (dir robot))
