{-# LANGUAGE FlexibleContexts #-}
module TicTacToe where

import Data.Char
import Data.Maybe
import Data.List
import Text.Read hiding (get, Char)

import Data.Char

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont

import Graphics.UI.GLUT hiding (Vertex)
import Data.IORef (newIORef)

-------------------------------------------------------------------
data Player = O | X
            deriving (Eq, Show)

data Cell = Empty | Taken Player
          deriving (Eq)

type Board = ([Cell], Int)

type Pos = (Int, Int)

-------------------------------------------------------------------

--
-- Some useful functions from, or based on, the unassessed problem sheets...
--

-- Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x)
  | p x = m
filterMaybe p _
  = Nothing

-- Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace 0 p (c : cs)
  = p : cs
replace _ p []
  = []
replace n p (c : cs)
  = c : replace (n - 1) p cs

-- Returns the rows of a given board.
rows :: Board -> [[Cell]]
rows (cs , n)
  = rows' cs
  where 
    rows' []
      = []
    rows' cs
      = r : rows' rs
      where
        (r, rs) = splitAt n cs

-- Returns the columns of a given board.
cols :: Board -> [[Cell]]
cols
  = transpose . rows

-- Returns the diagonals of a given board.
diags :: Board -> [[Cell]]
diags (cs, n)
  = map (map (cs !!)) [[k * (n + 1) | k <- [0 .. n - 1]],
                      [k * (n - 1) | k <- [1 .. n]]]

-------------------------------------------------------------------

gameOver :: Board -> Bool
gameOver b
  = rs || cs || ds
  where
    check (Empty:_) = False
    check cs = (null . tail . nub) cs
    rs = any check (rows b)
    cs = any check (cols b)
    ds = any check (diags b)

-------------------------------------------------------------------

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--
parsePosition :: String -> Maybe Pos
parsePosition s
  = readMaybe s'' :: Maybe Pos
  where
    s' = map (\x -> if x == ' ' then ',' else x) s
    s'' = '(' : s' ++ ")"

tryMove :: Player -> Pos -> Board -> Maybe Board
tryMove p (x, y) b@(c, n)
  | x < 0 || x >= n = Nothing
  | y < 0 || y >= n = Nothing
  | c!!i == Empty   = Just (nb, n)
  | otherwise       = Nothing
  where
    i  = n * x + y
    nb = replace i (Taken p) c

    

-------------------------------------------------------------------
-- I/O Functions

instance Show Cell where
  show (Taken O) = "O"
  show (Taken X) = "X"
  show Empty     = " "

printRow :: [Cell] -> IO ()
printRow r
  = do
    let cells = intercalate " | " $ map show r
        n = 4 * length r - 3
        sep = concat $ replicate n "-"
    
    putStrLn cells
    putStrLn sep

prettyPrint :: Board -> IO ()
prettyPrint b
  = do
    mapM_ printRow (rows b)
    putStrLn ""

-- The following reflect the suggested structure, but you can manage the game
-- in any way you see fit.

-- Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).
takeTurn :: Board -> Player -> IO Board
takeTurn b p
  =  do
      putStr "Enter a position (row, col): "
      strpos <- getLine

      let pos = parsePosition strpos

      if null pos
      then do
        putStrLn "Invalid position!"
        takeTurn b p
      else do
        let apos = fromJust pos

        let nb = tryMove p apos b

        if null nb
        then do
          putStrLn "Invalid position!"
          takeTurn b p
        else do
          return $ fromJust nb


-- Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
playGame :: Board -> Player -> IO ()
playGame b p
  = do
    prettyPrint b

    nb <- takeTurn b p

    if gameOver nb
    then do
      prettyPrint nb
      putStrLn $ "Player " ++ show p ++ " wins!"
    else do
      let np = if p == O then X else O
      playGame nb np

getYesNo :: String -> IO Bool
getYesNo p
  = do
    putStr p

    s <- getLine

    if s == "Y" || s == "y" then
      return True
    else
      if s == "N" || s == "n" then
        return False
      else do
        putStrLn "Invalid Input!"
        getYesNo p


-- Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main
  = do
    putStrLn "Enter the board size: "
    n <- getLine

    gui <- getYesNo "Would you like to use the GUI? (Y/N): "

    let m = read n :: Int
        board = replicate (m*m) Empty 

    if gui then
      mainGUI m
    else
      playGame (board, m) X

-------------------------------------------------------------------

-- Extention
-- Add a GUI

mainGUI :: Int -> IO ()
mainGUI n
  = do
    _ <- getArgsAndInitialize
    w <- createWindow "TicTacToe"

    width <- newIORef (300::GLsizei)
    height <- newIORef (300::GLsizei)
    board <- newIORef (replicate (n*n) Empty, n::Int)
    player <- newIORef O
      
    actionOnWindowClose   $= ContinueExecution
    displayCallback       $= do
                              w <- get width
                              h <- get height
                              b <- get board
                              p <- get player
                              let d = drawGame b p (fromIntegral w) (fromIntegral h)
                              display d
    keyboardMouseCallback $= Just (keyboardMouse board player width height)
    reshapeCallback       $= Just (reshape (width, height))

    mainLoop

keyboardMouse :: (HasUpdate t Board Board, HasUpdate k Player Player, HasGetter k Player, HasGetter t Board, HasGetter s GLsizei) => t -> k -> s -> s -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse _ _ _ _ (Char '\ESC') Down _ _ 
  = leaveMainLoop

keyboardMouse b' p' w' h' (MouseButton _) Down _ (Position x' y')
  = do
    b <- get b'
    p <- get p'
    w'' <- get w'
    h'' <- get h'

    let (c, n) = b
        x = fromIntegral x'
        y = fromIntegral y'
        w = fromIntegral w''
        h = fromIntegral h''
        mx = fromIntegral (div x $ div w n)
        my = fromIntegral (div y $ div h n)

    let nb' = tryMove p (my, mx) b

    if null nb'
    then do
      return ()
    else do
      let nb = fromJust nb'
          win = gameOver nb
      if win
      then do
        putStrLn "You win!"
        postRedisplay Nothing
      else do
        let np = if p == X then O else X
        b' $~! const nb
        p' $~! const np

        postRedisplay Nothing

keyboardMouse _ _ _ _ _ _ _ _
  = do
    return ()

drawGame :: Board -> Player -> Double -> Double -> [ColouredLine]
drawGame (b, n) p w h
  = grid ++ cells
  where
    grid = drawGrid n w h
    m = fromIntegral n
    dw = w / m
    dh = h / m
    cells = concat [drawCell (b!!(round i*n + round j)) (dw/2 + j*dw) (h - dh/2 - i*dh) dw dh | i <- [0..m-1], j <- [0..m-1]]

drawGrid :: Int -> Double -> Double -> [ColouredLine]
drawGrid n w h
  = v ++ h'
  where
    m = fromIntegral n
    wp = w / m
    hp = h / m
    col = (1, 1, 1)

    vp = [hp * i | i <- [1..m-1]]
    v = [((0, y), (w, y), col) | y <- vp]

    hpp = [wp * i | i <- [1..m-1]]
    h' = [((x, 0), (x, h), col) | x <- hpp]

drawCell :: Cell -> Double -> Double -> Double -> Double -> [ColouredLine]
drawCell Empty _ _ _ _= []
drawCell (Taken O) x y w h = drawO x y w h
drawCell (Taken X) x y w h = drawX x y w h

drawX :: Double -> Double -> Double -> Double -> [ColouredLine]
drawX x y w h
  = [d1, d2]
  where
    z = min w h
    tr = (x + 0.4 * z, y - 0.4 * z)
    tl = (x - 0.4 * z, y - 0.4 * z)
    br = (x + 0.4 * z, y + 0.4 * z)
    bl = (x - 0.4 * z, y + 0.4 * z)

    col = (1, 0, 0)

    d1 = (tl, br, col)
    d2 = (bl, tr, col)

drawO :: Double -> Double -> Double -> Double -> [ColouredLine]
drawO x y w h
  = lines
  where
    r = min w h * 0.4
    n = 20.0
    points = [(x + r * cos (2*pi*i/n), y + r * sin (2*pi*i/n)) | i <- [0..n]]

    col = (0, 0, 1)
    lines = [(points!!i, points!!(i-1), col) | i <- [1..length points-1]]




-- below here is code from the lsystems Graphics.hs file that is (mostly) unmodified

type Colour
  = (Float, Float, Float)

type Vertex
  = (Double, Double)

type ColouredLine
  = (Vertex, Vertex, Colour)


display :: [ColouredLine] -> IO ()
display ls
  = do
      clear [ColorBuffer]

      let verticesOf (from, to, _)      = [from, to]
          vertices                      = concatMap verticesOf ls
          ((minX, minY), (maxX, maxY))  = computeScale vertices

      preservingMatrix $ do
        ortho (realToFrac minX) (realToFrac maxX)
          (realToFrac minY) (realToFrac maxY)
          (0) (1)

        renderPrimitive Lines $ mapM_ lineVertices ls

      flush

lineVertices :: ColouredLine -> IO ()
lineVertices ((fromX, fromY), (toX, toY), (r, g, b))
  = do
      color $ Color3 (realToFrac r :: GLfloat) (realToFrac g) (realToFrac b)
      vertex $ Vertex3 (realToFrac fromX :: GLfloat) (realToFrac fromY) 0
      vertex $ Vertex3 (realToFrac toX :: GLfloat) (realToFrac toY) 0

computeScale :: [Vertex] -> (Vertex, Vertex)
computeScale 
  = foldl' f ((infinity, infinity), (-infinity, -infinity)) 
    where
      f ((minX, minY), (maxX, maxY)) (x, y)
        = ((min minX x, min minY y), (max maxX x, max maxY y))

      infinity
        = 10000000

reshape :: (HasUpdate t b b, Num b) => (t, t) -> Size -> IO ()
reshape (w, h) s@(Size x y)
  = do
      w $~! const (fromIntegral x)
      h $~! const (fromIntegral y)
      viewport $= (Position 0 0, s)
      postRedisplay Nothing

-------------------------------------------------------------------

testBoard1, testBoard2, testBoard3 :: Board

testBoard1
  = ([Taken O,Taken X,Empty,Taken O,
      Taken O,Empty,Taken X,Taken X,
      Taken O,Empty,Empty,Taken X,
      Taken O,Taken X,Empty,Empty],
      4)

testBoard2
  = ([Taken X,Empty,
      Empty,Empty],
      2)

testBoard3
  = ([Taken O,Taken X,Empty,Taken O,Taken X,
      Taken O,Empty,Taken X,Taken X,Empty,
      Empty,Empty,Taken X,Taken O,Taken O,
      Taken O,Taken X,Empty,Empty,Taken X,
      Taken X,Empty,Taken O,Empty,Empty],
      5)
