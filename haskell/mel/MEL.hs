module MEL where

import World

-- | MEL interpreter data types
data Relative = Ahead | ToLeft | ToRight | Behind deriving (Eq, Show)

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
                                ToLeft  -> pred dir
                                ToRight -> succ dir
                                Behind  -> (succ . succ) dir

-- | Notes: The condition AtGoalPos is true if the robot is in the 
-- top right corner of the maze, otherwise it is false

-- | Modelling the state of a robot
data Robot = Robot { pos  :: Position
                   , dir  :: Direction
                   , hist :: [Position]
                   } deriving (Show, Eq)

type World = (Maze, Robot)

initialWorld :: Maze -> World
initialWorld maze = (maze, initialRobot)

-- | Begin robotting
initialRobot :: Robot
initialRobot = Robot (0,0) North []


-- | The runRC function consumes a world and produces 
newtype RobotCommand a = RC { runRC :: (Maze, Robot) -> Either Robot (a, Robot) }

inject :: a -> RobotCommand a
inject a = RC $ \(_,r) -> Right (a,r)

-- | We want to chain two robot commands
chain :: RobotCommand a -> (a -> RobotCommand b) -> RobotCommand b
chain (RC h) f = RC $ \w@(m,r) -> do (a, r') <- h w
                                     let (RC g) = f a
                                     g (m, r')
  
instance Monad RobotCommand where
  return = inject
  (>>=)  = chain

interp :: Stm -> RobotCommand ()
interp stm = RC $ \world@(maze, robot) -> case stm of
  TurnRight ->
    let robot' = Robot (pos robot) (succ (dir robot)) (hist robot)
    in Right ((), robot')

  TurnLeft ->
    let robot' = Robot (pos robot) (pred (dir robot)) (hist robot)
    in Right ((), robot')

  Forward ->
    let d = dir robot
        p = pos robot
        np = neighbor p d
    in if validMove maze p np
       then Right ((), Robot np d (p : hist robot))
       else Left robot
  
  Backward -> 
    let d = dir robot
        p = pos robot
        np = neighbor p $ (succ . succ) d
    in if validMove maze p np
       then Right ((), Robot np d (p : hist robot))
       else Left robot
  
  If cond true false -> do
    let (RC a) = interp $ if evalCond world cond then true else false
    a world

  While cond stm -> do
    let (RC a) = interp $ Block $ if evalCond world cond then [stm, (While cond stm)] else []
    a world

  Block []         -> Right ((), robot)
  Block (stm:stms) -> do
    let (RC a) = interp stm
    (_, r') <- a world
    let (RC b) = interp $ Block stms
    b (maze,r')

evalCond :: World -> Cond -> Bool
evalCond w@(m,r) cond = case cond of
  (Wall rel)  -> (getRelDir rel (dir r)) `elem` (getCell m (pos r))
  (And c1 c2) -> (evalCond w c1) && (evalCond w c2)
  (Not c)     -> not (evalCond w c)
  AtGoalPos   -> (pos r) == getGoalPos m
  Visited pos -> pos `elem` (hist r)


data Result a = Success a | Failure a deriving (Show, Eq)

runProg :: Maze -> Program -> Result ([Position], Direction)
runProg maze program = let (RC prog) = interp program
                       in case (prog $ initialWorld maze) of
                         (Right (_, r)) -> Success $ status r
                         (Left r)       -> Failure $ status r
  where status robot = ((pos robot):(hist robot), (dir robot))

