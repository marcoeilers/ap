module SoilParserTest where

import Test.HUnit
import SimpleReadP
import SoilParser
import SoilAst
import qualified Data.List as L

testAll :: IO Counts
testAll = runTestTT unitTests

unitTests :: Test
unitTests = TestList [ parseName
                     , parseIdent
                     , parseExample
                     , parseMinimalWhitespace
                     , parseNoWhitespace
                     , parseMaximalWhitespace
                     , parseCaseEmpty
                     , parseConcat
                     , parseIf
                     , parseFunction
                     , testErrors
                     , testErrors2
                     , compareParserAstHello
                     , compareParserAstClean
                     , compareParserAstGate ]

parseName :: Test
parseName = TestCase $ do
    let p  = parseEof name "valid_Name3"
        p2 = parseEof name "StartsCapital"
        p3 = parseEof name "2startsNumber"  -- must fail (may not start with number)
        p4 = parseEof name "self"           -- must fail (reserved word)
        p5 = parseEof name "special$chars"  -- must fail (only alphanums allowed)
        p6 = parseEof name "#thisIsAnIdent" -- must fail (# marks an ident)
    assertBool "Make sure name parser adheres to rules"
               ((p, p2, p3, p4, p5, p6) == 
               ([("valid_Name3","")], [("StartsCapital","")], [], [], [], []))

parseIdent :: Test
parseIdent = TestCase $ do
    let p = parseEof ident "#_valid"
        p2 = parseEof ident "#Valid"
        p3 = parseEof ident "#3Valid"
        p4 = parseEof ident "#then"   -- idents may contain reserved words after #
        p5 = parseEof ident "name"    -- must fail (no ident without #)
    assertBool "Makes sure ident parser adheres to rules"
               ((p, p2, p3, p4, p5) == 
               ([("_valid","")], [("Valid","")], [("3Valid","")], [("then","")], []))

parseExample :: Test
parseExample = TestCase $ do
    let p = parseString "send (#ok) to self"
    assertBool "Parsing given example: send (#ok) to self" 
               (p == Right ([], [SendTo [Id "ok"] Self]))

parseMinimalWhitespace :: Test
parseMinimalWhitespace = TestCase $ do
    let p = parseString "create#proc with#function(first,#second)"
    assertBool "Parsing create statement with minimal whitespace" 
               (p == Right ([], [Create (Id "proc") (Id "function") [Par "first", Id "second"]]))

parseNoWhitespace :: Test
parseNoWhitespace = TestCase $ do
    let p = parseString "send(#ok)toself"
    assertBool "Parsing send statement with missing" 
               (p == Right ([], [SendTo [Id "ok"] Self]))

parseMaximalWhitespace :: Test
parseMaximalWhitespace = TestCase $ do
    let p = parseString "\n\nbecome\t\n  #func \t(\n #ident\t,  name\n\t, \tself \n) "
    assertBool "Parsing become statement with lots of whitespace" 
               (p == Right ([], [Become (Id "func") [Id "ident", Par "name", Self]]))

parseCaseEmpty :: Test
parseCaseEmpty = TestCase $ do
    let p = parseEof expr "case msg of _ : end"
    assertBool "Parse case expression with no cases and empty fallback case"
               (p== [(CaseOf (Par "msg") [] (Acts []), "")])

parseConcat :: Test
parseConcat = TestCase $ do
    let p = parseString "send (#ok) to #first concat second concat #third concat self"
    assertBool "Testing left-associativity of concat" 
               (p == Right ([], [SendTo [Id "ok"] 
                         (Concat (Concat (Concat (Id "first") (Par "second")) (Id "third") ) Self)]))

parseIf :: Test
parseIf = TestCase $ do
    let p = parseEof expr "if #first == second then else end"
    assertBool "Parsing simple if statement"
               (p == [(IfEq (Id "first") (Par "second") (Acts []) (Acts []),"")])

parseFunction :: Test
parseFunction = TestCase $ do
    let p = parseEof func "let #func(first, second) from msg = end"
    assertBool "Parsing simple function"
               (p == [(Func {funcname = "func", 
                             params = ["first","second"], 
                             receive = "msg", 
                             body = Acts []},"")])

testErrors :: Test 
testErrors = TestCase $ do
    let p = parseString "\n send (#repeat) \n \t to #repeater a send (#stop) \n to #repeater2"
        (Left (ParseErrorAfter (l,c))) = p
    assertBool "Checking error message"
               (l == 3 && c >= 14) 

testErrors2 :: Test 
testErrors2 = TestCase $ do
    let p = parseString "\n create #repeater \n \t with #repeat \n(#first, #second) notaToken "
        (Left (ParseErrorAfter (l,c))) = p
    assertBool "Checking error message"
               (l == 4 && c >= 17)

compareParserAstHello :: Test
compareParserAstHello = compareParserAst "examples/helloWorld.soil" "examples/helloWorld.soilAst" 

compareParserAstClean :: Test
compareParserAstClean = compareParserAst "examples/cleanUp.soil" "examples/cleanUp.soilAst"

compareParserAstGate :: Test
compareParserAstGate = compareParserAst "examples/gateKeeper.soil" "examples/gateKeeper.soilAst"

compareParserAst :: FilePath -> FilePath -> Test
compareParserAst f fast = TestCase $ do 
    s <- readFile f
    sast <- readFile fast
    let (Right r) = parseString s
        cleansast = unwords $ lexAll sast
        rast = read cleansast :: Program
    assertBool ("Comparing supplied AST with parsed one for " ++ f) (rast == r)

--
-- Utility functions
--

lexAll :: String -> [String]
lexAll s = 
    let ((result, rest):_) = lex s
    in case rest of
        "" -> [result]
        _  -> result : lexAll rest
