{-
  Example code produced during Advanced Programming lecture 1.

  Code for tic-tac-toe games

  Date: Sep 4, 2012
  Author: Ken Friis Larsen <kflarsen@diku.dk>
-}


data Player = Cross | Nought
            deriving (Show, Eq)
                     
data Cell = Move Player | Empty
          deriving (Show, Eq)

type Row = [Cell]
type Board = [Row]

emptyBoard :: Board
emptyBoard = take 3 emptyRows
  where emptyRows = repeat emptyRow
        emptyRow = take 3 (repeat Empty)

type Position = (Int, Int)

-- The move function we wrote during the lecture
move :: Position -> Board -> Player -> Board
move (x, y) board player = rowsBefore ++ (changed : rowsAfter)
  where (rowsBefore, toBeChanged : rowsAfter) = splitAt x board
        (cellsBefore, old : cellsAfter) = splitAt y toBeChanged
        changed = cellsBefore ++ (Move player : cellsAfter)
        

-- Type for representing the state of a game. That is, which should
-- take the next move, and what does the board look like.
type GameState = (Player, Board)

startState :: GameState
startState = (Cross, emptyBoard)

-- The move function I would suggest that we go with
makeMove :: Position -> GameState -> GameState
makeMove pos (player, board) = (nextPlayer, newBoard)
  where nextPlayer = if player == Cross then Nought else Cross
        newBoard = move pos board player
                   


validMove :: Position -> GameState -> Bool
validMove (x,y) (player, board) = getCell (x,y) board == Empty
  where getCell (x,y) board = split y $ split x board
        split i = (curry $ head . snd . (uncurry splitAt)) i


allMoves :: [Position]
allMoves = [ (x,y) | x <- [0..2], y <- [0..2] ]

allValidMoves :: GameState -> [Position]
allValidMoves gs = [ pos | pos <- allMoves, validMove pos gs ]

-- The type for representing game trees.  Each node have a game state
-- and a list of *valid* moves leading to (sub-) game trees
data GameTree = Node GameState [(Position, GameTree)]

-- Grow the game tree starting in a given state
makeTree :: GameState -> GameTree
makeTree gs = Node gs [ (pos, subTree pos) | pos <- allValidMoves gs ]
  where subTree pos = makeTree $ makeMove pos gs


-- A much smaller state 
testMakeTree = makeTree gs
  where gs = foldr makeMove startState [ (x,y) | x <- [0..2], y <- [0..1] ]

-- Return all game states in a game tree, duplicates are allowed
allNodes :: GameTree -> [GameState]
allNodes (Node gs subs) = gs : concatMap (allNodes . snd) subs

-- Observe the difference in running time of 
--    length $ allNodes $ makeTree startState    -- should return 986410
-- and
--    take 3 $ allNodes $ makeTree startState
-- Can you explain that?
