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
                                ToLeft  -> pred dir
                                ToRight -> succ dir
                                Behind  -> (succ . succ) dir

initialWorld :: Maze -> World
initialWorld maze = (maze, initialRobot)

-- | A new Robot in the lower left corner looking North
--   Should always be used when constructing new robots.
initialRobot :: Robot
initialRobot = Robot (0,0) North []


-- | The runRC function consumes a world and produces 
newtype RobotCommand a = RC { runRC :: (Maze, Robot) -> Either Robot (a, Robot) }

{- Monadic laws:
These must be satisfied 

Left id: return a >>= f === f a

Right id: m >>= return === m

Associativity: (m >>= f) >>= g === m >>= (\x -> f x >>= g)

-}

inject :: a -> RobotCommand a
inject a = RC $ \(_,r) -> Right (a,r)

-- | We want to chain two robot commands
-- Using the Either monad inside
chain :: RobotCommand a -> (a -> RobotCommand b) -> RobotCommand b
chain (RC h) f = RC $ \w@(m,r) -> do (a, r') <- h w
                                     let (RC g) = f a
                                     g (m, r')
-- | Our central monad
instance Monad RobotCommand where
  return = inject
  (>>=)  = chain

-- | Interprets statements into robotCommands.
-- RobotCommand can then be applied to a world to get a result.
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

