-- | Parser for Soil source code.
--   Reads source code either from a file or from a string
--   and returns an AST of the soil program
module SoilParser ( parseString 
                  , parseFile 
                  -- only exported for testing purposes
                  , Error(..)
                  , name
                  , ident
                  , expr
                  , func
                  , program )
       where

import Text.ParserCombinators.ReadP
import Data.Char(isSpace,isLower,isUpper,isDigit,isAlpha,isAlphaNum)
import Control.Applicative((<$>),(<*>))
import SoilAst
import SimpleReadP
import qualified Data.List as L

-- | Parses Soil source code from a string and returns either the AST or an error
parseString :: String               -- ^ A string containing Soil source code
            -> Either Error Program -- ^ Either the resulting AST or an error
parseString s = case res of
    [] -> Left $ ParseErrorAfter (findError s)
    [(p, _)]   -> Right p
    (r:rs)     -> error "Ambiguous parsing result; this should never happen"
    where res = parseEof program s
          (r:rs) = res

-- | Reads in a file, parses the contained source code and prints the AST (or an error)
parseFile :: FilePath                  -- ^ Path to the file containing the Soil code
          -> IO (Either Error Program) -- ^ Either the resulting AST or an error
parseFile f = do 
    s <- readFile f
    let r = parseString s
    print $ show r
    return r


------------------------
-- Parser
------------------------

-- | Reserved words of Soil
--   Names may not be equal to any one of these.
reservedWords :: [String]
reservedWords = [ "let"
                 ,"from"
                 ,"case"
                 ,"of"
                 ,"end"
                 ,"self"
                 ,"concat"
                 ,"if"
                 ,"then"
                 ,"else"
                 ,"send"
                 ,"to"
                 ,"create"
                 ,"become"
                 ,"with"]

-- | Consumes a hash, returns all alphanumerics and underscores that follow
--   Does NOT return the #
ident :: Parser Ident
ident = token $ do 
    satisfy isHash
    munch1 ((||) <$> isAlphaNum <*> (==) '_')

-- | Starts with a letter, then all following alphanumerics and underscores.
--   Result may not be identical to any of the reserved words.
name :: Parser Name
name = token $ do 
    c <- satisfy isAlpha
    cs <- munch ((||) <$> isAlphaNum <*> (==) '_')
    if (c:cs) `elem` reservedWords || isHash c
        then pfail 
        else return $ c:cs

isHash :: Char -> Bool
isHash c = c=='#'


-- | Use chainl1 to enforce correct AST for left-associative concat.
prim :: Parser Prim
prim = chainl1 simpleprim (symbol "concat" >> return Concat)


simpleprim :: Parser Prim
simpleprim = idt
         <|> par
         <|> self

idt :: Parser Prim
idt = do 
    i <- ident
    return $ Id i

par :: Parser Prim
par = do 
  n <- name
  return $ Par n

self :: Parser Prim
self = do 
    symbol "self"
    return Self

conc :: Parser Prim
conc = do 
    p1 <- simpleprim
    symbol "concat"
    p2 <- prim
    return $ Concat p1 p2

-- | Parser for the entirety of a Soil program.
program :: Parser Program
program = do 
    funcs <- many func
    actops <- many actop
    return (funcs, actops)

func :: Parser Func
func = do 
    symbol "let"
    fn <- ident
    prms <- between (schar '(') (schar ')') (sepBy name (schar ','))
    symbol "from"
    rec <- name
    symbol "="
    bd <- expr
    symbol "end"
    return Func {funcname = fn,
                   params = prms, 
                   receive = rec,
                   body = bd}

expr :: Parser Expr
expr = caseExp 
   <|> ifEq
   <|> acts

caseExp :: Parser Expr
caseExp = do 
    symbol "case"
    pr <- prim
    symbol "of" 
    (first, second) <- cases 
    symbol "end"
    return $ CaseOf pr first second

cases :: Parser ([([Name], Expr)], Expr)
cases = do 
    first <- many oneCase
    symbol "_"
    symbol ":"
    e <- expr
    return (first, e)

oneCase :: Parser ([Name], Expr)
oneCase = do 
    params <- between (schar '(') (schar ')') (sepBy name (schar ','))
    symbol ":"
    e <- expr
    return (params, e)

ifEq :: Parser Expr
ifEq = do 
    symbol "if"
    first <- prim
    symbol "=="
    second <- prim
    symbol "then"
    thenExp <- expr
    symbol "else"
    elseExp <- expr
    symbol "end"
    return $ IfEq first second thenExp elseExp

acts :: Parser Expr
acts = do 
    as <- many actop
    return $ Acts as

actop :: Parser ActOp
actop = send
    <|> create
    <|> become

send :: Parser ActOp
send = do 
    symbol "send"
    args <- between (schar '(') (schar ')') (sepBy prim (schar ','))
    symbol "to"
    p <- prim
    return $ SendTo args p

create :: Parser ActOp
create = do 
    symbol "create"
    p <- prim
    symbol "with"
    (p2, a) <- fcall
    return $ Create p p2 a

args :: Parser [Prim]
args = sepBy prim (schar ',')

fcall :: Parser (Prim, [Prim])
fcall = do 
    p <- prim
    a <- between (schar '(') (schar ')') args
    return (p, a)

become :: Parser ActOp
become = do 
    symbol "become"
    (p, a) <- fcall
    return $ Become p a

-- | An error contains the information up until which point (line and column) 
--   the source code has been parsed correctly; i.e. any errors in the source 
--   code which prevent it from being parsed correctly are after this point.
data Error = ParseErrorAfter (Int, Int) 
             deriving (Eq, Show)

-- | Determines up to which point (line and column) a given string can be 
--   parsed as a program
findError :: String -> (Int, Int)
findError s = findMostConsumed res 0 (1,1) s 
    where res = parse program s

-- | Of all the results of parse, finds the one which has consumed most of 
--   the input, finds the line and column up until which it has been consumed
--   returns these.
findMostConsumed :: [(Program, String)] -> Int -> (Int, Int) -> String -> (Int, Int)
findMostConsumed [] _ (l,c) _ = (l,c)
findMostConsumed (x:xs) most (l,c) all = 
    if consumed > most
    then let newPos = (calculateLine consumed 1 1 all)
         in findMostConsumed xs consumed newPos all
    else findMostConsumed xs most (l,c) all
    where (p, rest) = x
          consumed = length all - length rest

-- | Finds the line and column of the nth character in a given string
calculateLine :: Int -> Int -> Int -> String -> (Int, Int)
calculateLine 0 l c _ = (l, c)
calculateLine left l c [] = (l, c) -- this can never happen
calculateLine left l c (x:xs) = 
    if x == '\n'
    then calculateLine (left-1) (l+1) 1 xs
    else calculateLine (left-1) l (c+1) xs                     
