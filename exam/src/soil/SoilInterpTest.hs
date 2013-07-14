module SoilInterpTest where

import Test.HUnit
import SoilInterp
import SoilAst
import Debug.Trace
import qualified Data.Map as M

testAll :: IO Counts
testAll = runTestTT unitTests

unitTests :: Test
unitTests = TestList [ testIdent
                     , testName
                     , testSelf
                     , testConcat
                     , testConcat2
                     , testActs
                     , testCaseOf
                     , testIfTrue
                     , testIfFalse
                     , testErrors
                     , testSchedulerRR
                     , testSchedulerAll
                     , testHelloWorldRR
                     , testLooneyTunesRR
                     , testGatekeeperRR
                     , testHelloWorldAll]

testSelf :: Test
testSelf = testPrim Self before (Just ["proc"])
    where before = initialState {process = Just "proc"}

testName :: Test
testName = testPrim (Par "n") before (Just ["this", "that"])
    where before = initialState {names = NameEnv names}
          names  = M.insert "n" ["this", "that"] M.empty

testIdent :: Test
testIdent = testPrim (Id "tralala") initialState (Just ["tralala"])

testConcat :: Test
testConcat = testPrim (Concat (Par "aName") (Id "lalelu")) before Nothing
    where before = initialState {names = NameEnv names}
          names  = M.insert "aName" ["this", "that"] M.empty

testConcat2 :: Test
testConcat2 = testPrim (Concat (Par "aName") (Id "lalelu")) before (Just ["thislalelu"])
    where before = initialState {names = NameEnv names}
          names  = M.insert "aName" ["this"] M.empty


testActs :: Test
testActs = testExpr e before after
    where e = (Acts [SendTo [Id "message"] (Id "tester"), SendTo [Id "message2"] (Id "tester")])
          before = initialState {queue = Queue[("tester", emptyProcess)]}
          after  = initialState {queue = Queue[("tester", emptyProcess 
                                                   {mailbox = [["message"], ["message2"]]})]}

testIfTrue :: Test
testIfTrue = testExpr e before after
    where e = (IfEq (Id "abc") (Id "abc") 
              (Acts [SendTo [Id "then"] (Id "tester")])
              (Acts [SendTo [Id "else"] (Id "tester")]))
          before = initialState {queue = Queue[("tester", emptyProcess)]}
          after  = initialState {queue = Queue[("tester", emptyProcess {mailbox = [["then"]]})]}

testIfFalse :: Test
testIfFalse = testExpr e before after
    where e = (IfEq (Id "abc") (Id "abd") 
              (Acts [SendTo [Id "then"] (Id "tester")])
              (Acts [SendTo [Id "else"] (Id "tester")]))
          before = initialState {queue = Queue[("tester", emptyProcess)]}
          after  = before {queue = Queue[("tester", emptyProcess {mailbox = [["else"]]})]}

testCaseOf :: Test
testCaseOf = testExpr e before after
    where e = (CaseOf (Par "name") 
              [(["first", "second"], Acts [SendTo [Par "first"] (Id "tester")])]
              (Acts [SendTo [Par "fallthrough"] (Id "tester")])) 
          names  = M.insert "name" ["result", "other"] M.empty
          names2 = M.insert "first" ["result"] names
          before = initialState {queue = Queue[("tester", emptyProcess)],
                                 names = NameEnv  names}
          after  = before {queue = Queue[("tester", emptyProcess {mailbox = [["result"]]})],
                           names = NameEnv $ M.insert "second" ["other"] names2}

testExpr :: Expr -> ProgramState -> ProgramState -> Test
testExpr e before after = TestCase $ do
    let prog     = evalExpr e
        (_, res) = runSP prog before
    assertBool "Test if the expression transforms a state correctly"
               (res == after)

testPrim :: Prim -> ProgramState -> Maybe [Ident] -> Test
testPrim p before after = TestCase $ do
    let prog = evalPrim p
        (res, _) = runSP prog before
    assertBool "Test if Prim is evaluated correctly"
               (res == after)

testErrors :: Test
testErrors = testParsedProgRR 20 "examples/errors.soil"
             (\r -> r == ([],["Create statement had wrong number of args for function."
                            ,"Create used unknown function id."
                            ,"Create operation uses existing process ID."]))

testSchedulerRR :: Test
testSchedulerRR = TestCase $ do
    let prog = nextProcessRR
        q = [("first", emptyProcess), ("second", emptyProcess), ("third", emptyProcess)]
        before = initialState {queue = Queue q}
        (res, s) = runSP prog before
    assertBool "Test if RR-scheduler returns correct Pid and adjusts process queue"
               (res == "first" 
                && queue s == (Queue [("second", emptyProcess), 
                                      ("third", emptyProcess), 
                                      ("first", emptyProcess)]))

testSchedulerAll :: Test
testSchedulerAll = TestCase $ do
    let prog = nextProcAll
        q = [("first", emptyProcess), ("second", mailProcess), 
             ("third", mailProcess), ("fourth", emptyProcess)]
        before = initialState {queue = Queue q}
        (res, s) = runSP prog before
    assertBool "Test if All-scheduler returns correct Pids"
               (res == ["second", "third"] && s == before)

testHelloWorldRR :: Test
testHelloWorldRR = testParsedProgRR 20 "examples/helloWorld.soil"
                   (\r -> r == (["World","Hello"],[]))

testLooneyTunesRR :: Test
testLooneyTunesRR = testParsedProgRR 25 "examples/looneyTunes.soil"
                    (\r -> r == (["duck:season","BugsSays","wabbit:season","DuffySays"],[]))

testGatekeeperRR :: Test
testGatekeeperRR = testParsedProgRR 40 "examples/gateKeeper.soil"
                   (\r -> r == (["Hello:World"],
                                ["Sent message to unknown process foofoo"]))

testHelloWorldAll :: Test
testHelloWorldAll = testParsedProgAll 20 "examples/helloWorld.soil"
                    (\r -> r == [(["World","Hello"],[]) 
                                ,(["World","Hello"],[])
                                ,(["Hello", "World"],[])
                                ,(["Hello", "World"],[])])

testParsedProgRR :: Int -> FilePath -> (([String], [String]) -> Bool) -> Test
testParsedProgRR n f pred = TestCase $ do 
    r <- runFileRR n f
    assertBool "Check predicate for interpreter result (RR)" (pred r)

testParsedProgAll :: Int -> FilePath -> ([([String], [String])] -> Bool) -> Test
testParsedProgAll n f pred = TestCase $ do 
    r <- runFileAll n f
    assertBool "Check predicate for interpreter result (All)" (pred r)

--testExpression :: Expr -> ProgramState -> ProgramState -> TestCase

emptyProcess = Process {function = "",
                        arguments = [],
                        mailbox = []}

mailProcess = emptyProcess {mailbox = [["message"]]}


