

data Direction = North | East | South | West deriving Show

instance Enum Direction where
  succ North = East
  succ East  = South
  succ South = West
  succ West  = North
  
  toEnum 0    = North
  toEnum 5    = North
  toEnum 1    = East
  toEnum 2    = South
  toEnum 3    = West
  toEnum (-1) = West
  
  
  
  fromEnum North = 0
  fromEnum East  = 1
  fromEnum South = 2
  fromEnum West  = 3
  