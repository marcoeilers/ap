import World
import MEL
import Test.HUnit
import Test.QuickCheck as QC

-- | The test maze from the assignment text.
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

defaultWorld = initialWorld testMaze

testWorld pos dir = (testMaze, Robot pos dir []) 

-- | Function for starting programs from arbitrary position. Should ONLY be used
-- for testing purposes, otherwise runProg is the way to go.
testProg :: Position -> Direction -> Program -> MEL.Result ([Position], Direction) String
testProg p d program = let (RC prog) = interp program
                              in case (prog $ testWorld p d) of
                                 (Right (_, r)) -> MEL.Success $ status r
                                 (Left  (msg, r)) -> MEL.Failure (status r) msg
  where status robot = ((pos robot):(hist robot), (dir robot))

-- | In order for QuickCheck to generate positions
newtype LegalPosition = LP Position deriving (Eq, Show)

instance QC.Arbitrary LegalPosition where 
   arbitrary = QC.elements [LP (x,y) | x <- [0..4], y <- [0..4]]
   
instance QC.Arbitrary Direction where
   arbitrary = QC.elements [North, East, South, West]

-- | The wall follower algorithm specified here: http://en.wikipedia.org/wiki/Maze_solving_algorithm
-- Using the left-hand rule.
wallFollowProg = While (Not AtGoalPos)
                               (If (Wall ToLeft)
                                   (If (Wall Ahead) TurnRight Forward)
                                   (Block [TurnLeft, Forward])
                               )   
   
   
wallFollower (LP pos) dir = testProg pos dir wallFollowProg
                         
testWallfollower lopos dir = let result = wallFollower lopos dir
                             in case result of
                               MEL.Failure _ _             -> False
                               MEL.Success (pos:hist, dir) -> pos == (4,4)

----- Unit Tests

-- | Simple If tests, also check if the condition was evaluated correctly
testIf = TestCase $ assertBool "Test simple If command" $ 
         (MEL.Success ([(0,0)], East) == (runProg testMaze (If (Wall Ahead) TurnRight TurnLeft)))
         && evalCond (defaultWorld) (Wall Ahead)
         
testIf2 = TestCase $ assertBool "Test simple If command" $ 
          (MEL.Success ([(0,0)], West) == (runProg testMaze (If (Wall ToRight) TurnRight TurnLeft)))
          && not(evalCond (defaultWorld) (Wall ToRight))

-- | Empty block, should return input world
testEmptyBlock = TestCase $ assertBool "Test if empty block returns unchanged Robot" $
		 MEL.Success ([(0,0)], North) == runProg testMaze (Block [])  

-- | All basic moves 
testNavi = TestCase $ assertBool "Test some simple navigation" $ 
           MEL.Success ([(1,1),(1,0),(1,1),(1,0),(0,0)],North) == 
           (runProg testMaze ( Block [TurnRight, Forward, TurnLeft, Forward, Backward, Forward]))

-- | Simple while loop
testWhile = TestCase $ assertBool "Test simple While command" $ 
            MEL.Success ([(1,0),(0,0)],East) == runProg testMaze ( Block [ While (Wall Ahead) TurnLeft, Forward])

-- | Checks if running into a wall results in failure
testError = TestCase $ assertBool "Test if crossing walls fails and later commands are ignored" $ 
            MEL.Failure ([(0,0)], North) "Ran forward into a wall." == runProg testMaze (Block[Forward, TurnRight, Forward, Forward])

-- | Checks if one-cell maze is traversed correctly
testOneCell = TestCase $ assertBool "Test if robot traverses one-cell maze correctly" $
              MEL.Success([(0,0)],North) == runProg (fromList oneCellMaze) wallFollowProg
              
-- | Checks if simple 2x2 maze is traversed correctly
testFourCell = TestCase $ assertBool "Test if robot traverses four-cell maze correctly" $
               MEL.Success([(1,1),(1,0),(0,0)],North) == runProg (fromList fourCellMaze) wallFollowProg


-- | All unit tests 
unitTests = TestList [ testEmptyBlock
                     , testNavi
                     , testIf
                     , testIf2
                     , testWhile
                     , testError
                     , testOneCell
                     , testFourCell
                     ]

-- | Run all tests
testAll = do runTestTT unitTests
             quickCheck testWallfollower

oneCellMaze :: [(Position, Cell)]
oneCellMaze = [((0,0), [North, East, West, South])]

fourCellMaze :: [(Position, Cell)]
fourCellMaze = [ ((0,0), [South, West, North])
               , ((0,1), [North, West, South])
               , ((1,0), [South, East])
               , ((1,1), [North, East])]
