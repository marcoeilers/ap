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
wallFollowProg = While (Not AtGoalPos)
                               (If (Wall ToLeft)
                                   (If (Wall Ahead) TurnRight Forward)
                                   (Block [TurnLeft, Forward])
                               )   
   
   
wallFollower (LP pos) dir = testProg pos dir wallFollowProg
                         
testWallfollower lopos dir = let result = wallFollower lopos dir
                             in case result of
                               MEL.Failure _               -> False
                               MEL.Success (pos:hist, dir) -> pos == (4,4)

-- Test the ahead function
testIf = TestCase $ assertBool "Test simple If command" $ 
         (MEL.Success ([(0,0)], East) == (runProg testMaze (If (Wall Ahead) TurnRight TurnLeft)))
         && evalCond (defaultWorld) (Wall Ahead)
         
testIf2 = TestCase $ assertBool "Test simple If command" $ 
          (MEL.Success ([(0,0)], West) == (runProg testMaze (If (Wall ToRight) TurnRight TurnLeft)))
          && not(evalCond (defaultWorld) (Wall ToRight))

testEmptyBlock = TestCase $ assertBool "Test if empty block returns unchanged Robot" $
		 MEL.Success ([(0,0)], North) == runProg testMaze (Block [])
                  

testNavi = TestCase $ assertBool "Test some simple navigation" $ 
           MEL.Success ([(1,1),(1,0),(1,1),(1,0),(0,0)],North) == 
           (runProg testMaze ( Block [TurnRight, Forward, TurnLeft, Forward, Backward, Forward]))


testWhile = TestCase $ assertBool "Test simple While command" $ 
            MEL.Success ([(1,0),(0,0)],East) == runProg testMaze ( Block [ While (Wall Ahead) TurnLeft, Forward])

testError = TestCase $ assertBool "Test if crossing walls fails and later commands are ignored" $ 
            MEL.Failure([(0,0)], North) == runProg testMaze (Block[Forward, TurnRight, Forward, Forward])

successTests = TestList [testError, testIf, testIf2, testNavi, testWhile, testEmptyBlock, testGoodMaze]

errorTests = TestList [testBadMaze1, testBadMaze2, testBadMaze3, testBadMaze4]

testGoodMaze = TestCase $ assertBool "Test if well-formed maze is accepted" $
               let maze = fromList goodMaze 
               in getGoalPos maze == (1,1)

testBadMaze1 = TestCase $ assertBool "Test if mazes missing South walls are rejected" $
               let maze = fromList badMaze1 
               in getGoalPos maze == (1,1)
                  
testBadMaze2 = TestCase $ assertBool "Test if mazes missing East walls are rejected" $
               let maze = fromList badMaze2
               in getGoalPos maze == (1,1)
                  
testBadMaze3 = TestCase $ assertBool "Test if mazes with inconsistent neighbor walls are rejected" $
               let maze = fromList badMaze3
               in getGoalPos maze == (1,1)
                  
testBadMaze4 = TestCase $ assertBool "Test if mazes missing cells are rejected" $
               let maze = fromList badMaze4
               in getGoalPos maze == (1,1)

testAll = do
          runTestTT successTests
          quickCheck testWallfollower
          print "The following tests should all result in an Error"
          runTestTT errorTests

goodMaze :: [(Position, Cell)]
goodMaze = [((0,0), [West, South])
           ,((0,1), [West, North])
           ,((1,0), [East, South])
           ,((1,1), [East, North])]
          
badMaze1 :: [(Position, Cell)]
badMaze1 = [((0,0), [West])
           ,((0,1), [West, North])
           ,((1,0), [East, South])
           ,((1,1), [East, North])]
           
badMaze2 :: [(Position, Cell)]
badMaze2 = [((0,0), [West, South])
           ,((0,1), [West, North])
           ,((1,0), [South])
           ,((1,1), [East, North])]
           
badMaze3 :: [(Position, Cell)]
badMaze3 = [((0,0), [West, East, South])
           ,((0,1), [West, North])
           ,((1,0), [East, South])
           ,((1,1), [East, North])]

badMaze4 :: [(Position, Cell)]
badMaze4 = [((0,0), [West, South])
           ,((0,1), [West, North])
           ,((1,1), [East, North])]
           
