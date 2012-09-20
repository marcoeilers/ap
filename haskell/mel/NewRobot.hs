
import World

data Robot = Robot { pos  :: Position
                   , dir  :: Direction
                   , hist :: [Position]
                   } deriving (Show, Eq)


newtype RobotCommand a = RC { runRC :: (Maze, Robot) -> Either Robot (a, Robot) }

instance Monad RobotCommand where
  return a = RC $ \(_, r) -> Right (a, r)
  
  -- (>>=) :: RobotCommand a -> (a -> RobotCommand b) -> RobotCommand b
  rc1 >>= rc2 = RC $ \w@(m,r) -> do (a, r') <- runRC $ rc1 w
                                    
                                    let (RC g) = f a
                                    