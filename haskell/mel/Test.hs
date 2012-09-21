import World
import MEL
import Test.HUnit
import Test.QuickCheck as QC


defaultWorld = initialWorld testMaze

testWorld pos dir = (testMaze, Robot pos dir []) 

testProg :: Position -> Direction -> Program -> MEL.Result ([Position], Direction)
testProg p d program = let (RC prog) = interp program
                              in case (prog $ testWorld p d) of
                                 (Right (_, r)) -> MEL.Success $ status r
                                 (Left r)       -> MEL.Failure $ status r
  where status robot = ((pos robot):(hist robot), (dir robot))

newtype LegalPosition = LP Position deriving (Eq, Show)

instance QC.Arbitrary LegalPosition where 
   arbitrary = QC.elements [LP (x,y) | x <- [0..4], y <- [0..4]]
   
instance QC.Arbitrary Direction where
   arbitrary = QC.elements [North, East, South, West]

-- | The wall follower algorithm specified here: http://en.wikipedia.org/wiki/Maze_solving_algorithm
-- Using the left-hand rule.
wallFollower (LP pos) dir = testProg pos dir wallFollowProg
  where wallFollowProg = While (Not AtGoalPos)
                               (If (Wall ToLeft)
                                   (If (Wall Ahead) TurnRight Forward)
                                   (Block [TurnLeft, Forward])
                               )
                         
testWallfollower lopos dir = let result = wallFollower lopos dir
                             in case result of
                               MEL.Failure _               -> False
                               MEL.Success (pos:hist, dir) -> pos == (4,4)

-- Test the ahead function
testIf = TestCase $ assertBool "Test simple If command" $ 
         MEL.Success ([(0,0)], East) == (runProg testMaze (If (Wall Ahead) TurnRight TurnLeft))

testEmptyBlock = TestCase $ assertBool "Test if empty block returns unchanged Robot" $
		 MEL.Success ([(0,0)], North) == runProg testMaze (Block [])
                  

testNavi = TestCase $ assertBool "Test some simple navigation" $ 
           MEL.Success ([(1,1),(1,0),(1,1),(1,0),(0,0)],North) == 
           (runProg testMaze ( Block [TurnRight, Forward, TurnLeft, Forward, Backward, Forward]))


testWhile = TestCase $ assertBool "Test simple While command" $ 
            MEL.Success ([(1,0),(0,0)],East) == runProg testMaze ( Block [ While (Wall Ahead) TurnLeft, Forward])

testError = TestCase $ assertBool "Test if crossing walls fails and later commands are ignored" $ 
            MEL.Failure([(0,0)], North) == runProg testMaze (Block[Forward, TurnRight, Forward, Forward])

testMalformedMaze = TestCase $ assertdo fromList [((4,3), [])]

tests = TestList [testError, testIf, testNavi, testWhile, testEmptyBlock]

testAll = do
          runTestTT tests
          quickCheck testWallfollower



