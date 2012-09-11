{-
  Example code produced "during" Advanced Programming lecture 1.

  Whirlwind tour of Haskell

  Date: Sep 4, 2012
  Author: Ken Friis Larsen <kflarsen@diku.dk>
-}

simpsons :: [(String, Int)]  -- type signature
simpsons = [ ("Homer", 42)
           , ("Marge", 38)
           , ("Bart", 9)
           , ("Lisa", 8)
           , ("Maggie", 1)]

baz = (True, 23.5, "Homer, again")

s1 :: String
s1 = "Bart"

s2 :: [Char]
s2 = "Bart"

add :: Int -> Int -> Int
add x y = x+y

digits = [0..9]
evenDigits = [x | x <- digits, x `mod` 2 == 0]

nats = [0 ..]
evenNats = [x | x <- nats, x `mod` 2 == 0]

startFrom s = s : startFrom (s+1)

len [] = 0
len (_ : t) = 1 + len t

q [] = []
q (x:xs) = q sxs ++ [x] ++ q lxs
    where sxs = [a | a <- xs, a <= x]
          lxs = [b | b <- xs, b > x]

type Pos = (Int, Int)
data Direction = North | South | East | West
            deriving (Eq, Show)


data Student = Student {name :: String, knowsHaskell :: Bool}
               deriving (Eq, Show)


troels = Student {name="troels", knowsHaskell = False}


followAP :: Student -> Student
followAP stud = stud{knowsHaskell = True}


move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)

type Assoc a = [(String, a)]

findAssoc :: String -> Assoc a -> a
findAssoc key assoc = head bindings
    where bindings = [ val | (k, val) <- assoc, k == key]


data Expr = Con Int
          | Add Expr Expr
     deriving (Eq, Show, Read, Ord)

value :: Expr -> Int
value (Con n) = n
value (Add x y) = value x + value y

convList = map Con
add1 = Add $ Con 1