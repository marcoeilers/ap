import World

-- | MEL interpreter data types
data Relative = Ahead | ToLeft | ToRight | Behind deriving (Eq, Show)

data Cond = Wall Relative
          | And Cond Cond
          | Not Cond
          | AtGoalPos
          deriving (Eq, Show)

data Stm = Forward
         | Backward
         | TurnRight
         | TurnLeft
         | If Cond Stm Stm
         | While Cond Stm
         | Block [Stm]
         deriving (Eq, Show)

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

initialRobot :: Robot
initialRobot = Robot (0,0) North []


-- | The runRC function consumes a world and produces 
newtype RobotCommand a = RC { runRC :: (Maze, Robot) -> Maybe (a, Robot) }

instance Show (RobotCommand a) where
  show a = "foo"

inject :: a -> RobotCommand a
inject a = RC $ \(_,r) -> Just (a,r)

-- | We want to chain two robot commands
chain :: RobotCommand a -> (a -> RobotCommand b) -> RobotCommand b
chain (RC h) f = RC $ \w@(m,r) -> do (a, r') <- h w
                                     let (RC g) = f a
                                     g (m, r')
  
instance Monad RobotCommand where
  return = inject
  (>>=)  = chain


interp :: Stm -> RobotCommand ()
interp TurnRight = RC $ \(maze, robot) -> let robot' = Robot (pos robot) (succ (dir robot)) (hist robot)
                                          in Just ((), robot')
interp TurnLeft  = RC $ \(maze, robot) -> let robot' = Robot (pos robot) (pred (dir robot)) (hist robot)
                                          in Just ((), robot')
interp Forward   = RC $ \(maze, robot) -> let d = dir robot
                                              p = pos robot
                                              np = neighbor p d
                                          in if validMove maze p np
                                             then Just ((), Robot np d (np : hist robot))
                                             else Nothing
interp Backward  = RC $ \(maze, robot) -> let d = dir robot
                                              p = pos robot
                                              np = neighbor p $ (succ . succ) d
                                          in if validMove maze p np
                                             then Just ((), Robot np d (np : hist robot))
                                             else Nothing
interp (If cond true false) = RC $ \w@(m,r) -> -- must return Maybe ..if evalCond w cond then interp true else interp false
  do let (RC a) = if evalCond w cond then interp true else interp false
     a w
     

interp (While cond stm)  = undefined
interp (Block []) = RC $ \(maze, robot) -> Just ((), robot)
interp (Block (stm:stms)) = undefined

evalCond :: World -> Cond -> Bool
evalCond w@(m,r) cond = case cond of
  -- True if from current position there's a wall in the indicated direction (rel)
  (Wall rel)  -> (getRelDir rel (dir r)) `elem` (getCell m (pos r))
  (And c1 c2) -> (evalCond w c1) && (evalCond w c2)
  (Not c)     -> not (evalCond w c)
  AtGoalPos   -> (pos r) == getGoalPos m


testInterpIf = do let (RC f) = interp (If (Wall Ahead) TurnRight TurnLeft)
                  f (initialWorld testMaze)
                  