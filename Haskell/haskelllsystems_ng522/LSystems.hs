-- File for LSystems Graphics Contest
-- Run 'drawAniTree' with no arguments to generate the submission
-- Depending on the specs of the pc you run this on it might take a long time to
-- load and might run very very very slowly but will eventually produce the gif



{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module LSystems where

import Control.Monad
import Data.Char
import Data.List
import Graphics.UI.GLUT hiding (Vertex, Angle)
import Data.IORef (newIORef)

type Rule
  = (Char, String)

type Rules
  = [Rule]

type Angle
  = Float

type Axiom
  = String

type LSystem
  = (Angle, Axiom, Rules)

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Command
  = Char

type Commands
  = [Command]

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

----------------------------------------------------------
-- Functions for working with systems.

-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle (angle, axiom, rules)
  = angle

-- Returns the axiom string for the given system.
axiom :: LSystem -> String
axiom (angle, axiom, rules)
  = axiom

-- Returns the set of rules for the given system.
rules :: LSystem -> Rules
rules (angle, axiom, rules)
  = rules

-- Return the binding for the given character in the list of rules
lookupChar :: Char -> Rules -> String
-- Pre: the character has a binding in the Rules list
lookupChar c r
  = head [b | (a, b) <- r , a == c]

-- Expand command string s once using rule table r
expandOne :: String -> Rules -> String
expandOne s r
  = concat [lookupChar c r | c <- s]

-- Expand command string s n times using rule table r
expand :: String -> Int -> Rules -> String
expand s 0 _ = s
expand s n r
  = expand (expandOne s r) (n-1) r

radians :: Angle -> Angle
radians a = pi * (a / 180)

-- Move a turtle
move :: Command -> Angle -> TurtleState -> TurtleState
move 'F' a ((x, y), b) 
 = ((x + dx, y + dy), b)
 where
  c = radians b
  (dx, dy) = (cos c, sin c)

move 'L' a (v, b) = (v, b+a)
move 'R' a (v, b) = (v, b-a)
move _ a s = error "Unknown command"

--
-- Trace lines drawn by a turtle using the given colour, following the
-- commands in `cs' and assuming the given angle of rotation.
--
trace1 :: Commands -> Angle -> Colour -> [ColouredLine]
trace1 c a col
  = fst (trace1helper ((0, 0), 90) c)
  where
    trace1helper s [] = ([], "")
    trace1helper s ('[':cs)
      = (lines ++ x, y)
      where
        (lines, ccs) = trace1helper s cs
        (x, y)       = trace1helper s ccs
    trace1helper s@(v, _) (c:cs)
      | c == ']'  = ([], cs)
      | c == 'F'  = ((v, nv, col) : x, y)
      | otherwise = xy
      where
        ns@(nv, _) = move c a s
        xy@(x, y)  = trace1helper ns cs

trace2 :: Commands -> Angle -> Colour -> [ColouredLine]
trace2 commands a col
  = trace2helper ((0, 0), 90) [] commands
  where
    trace2helper s stack [] = []
    trace2helper s stack (c:cs)
      | c == '['  = trace2helper s (s:stack) cs
      | c == ']'  = trace2helper (head stack) (tail stack) cs
      | c == 'F'  = (fst s, nv, col) : trace2helper ns stack cs
      | otherwise = trace2helper ns stack cs
      where
        ns@(nv, _) = move c a s

----------------------------------------------------------
-- Some given functions

expandLSystem :: LSystem -> Int -> String
expandLSystem (_, axiom, rs) n
  = expandOne (expand axiom n rs) commandMap

drawLSystem1 :: LSystem -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (expandLSystem system n) (angle system) colour)

drawLSystem2 :: LSystem -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (expandLSystem system n) (angle system) colour)

----------------------------------------------------------
-- Extention functions

-- i suggest running drawAni, drawAntTree and drawAniRA in ghci
-- they look cool in my opinion

-- allow for some numeric operators on verticies
instance Num Vertex where
  (a, b) + (c, d) = (a + c, b + d)
  (a, b) - (c, d) = (a - c, b - d)

-- treats a given vertex as a vertor and rotates it 90 degrees anticlockwise
rot90 :: Vertex -> Vertex
rot90 (x, y) = (-y, x)

-- treats a given vertex as a vertor and rotates it 90 degrees clockwise
rot90' :: Vertex -> Vertex
rot90' (x, y) = (y, -x)


-- finds the midpoint of 2 vectors
midpoint :: Vertex -> Vertex -> Vertex
midpoint (a, b) (c, d) = ((a+c)/2, (b+d)/2)

-- given 2 verticies find the center of the circle such that the 2 points are
-- on the circumference and the angle at the center is 90 degrees
-- the extra vertex is used to work out which of the 2 possible centers is wanted
getCenter :: Vertex -> Vertex -> Vertex -> Vertex
getCenter a b k
  = v
  where
    m = midpoint a b
    mb = b - m
    h1 = m + rot90 mb
    h2 = m + rot90' mb
    v = if radius h1 k > radius h2 k then h1 else h2

-- given the center of a circle and a point on the circumference find the radius
radius :: Vertex -> Vertex -> Float
radius (a, b) (c, d) = sqrt((a-c)^2 + (b-d)^2)

-- given the center, radius, start angle, end angle and resolution
-- generate a set of n+1 evenly spaced points on the circumference of the given
-- circle between the start and end end angles which can be rendered as an arc
genPoints :: Vertex -> Float -> Float -> Float -> Float -> [Vertex]
genPoints (cx, cy) r a b n
  = [(x i, y i) | i <- [0..n]]
  where
    u = min a b
    v = max a b
    angle = if v-u < 2*pi+u-v then v-u else v-u-2*pi
    x i = cx + r * cos (u + angle * i/n)
    y i = cy + r * sin (u + angle * i/n)

-- given a set of points on an arc, generates a set of coloured lines that joins
-- them
genArc :: [Vertex] -> Colour -> [ColouredLine]
genArc vs col 
  = [(vs !! i, vs !! (i+1), col) | i <- [0..n]]
  where
    n = length vs - 2

-- takes in a vertex and returnes its angle between 0 and 2pi
getAngle :: Vertex -> Float
getAngle (x, 0)
  | x >= 0    = 0
  | otherwise = pi
getAngle (x, y)
  | x >= 0 && y > 0 = a
  | x < 0 && y > 0  = pi - a
  | x < 0 && y < 0  = pi + a
  | otherwise       = 2 * pi - a
  where
    a = atan (abs (y/x))

-- given 2 points returns the arc corresponding to the 1/4 circle that hits
-- both of the points
-- the extra vertex is used to work out which arc should be drawn since
-- there will be 2 possibilities
convertToArc :: Vertex -> Vertex -> Vertex -> Colour -> [ColouredLine]
convertToArc a b k
  = genArc v
  where
    c = getCenter a b k
    r = radius a c
    v = genPoints c r (getAngle (a-c)) (getAngle (b-c)) 12

-- checks if 3 points are colinear
colinear :: Vertex -> Vertex -> Vertex -> Bool
colinear a b c = abs (getAngle (b-a) - getAngle (c-a)) < 0.0001

-- takes in the lines produced by trace1/2 and converts into arcs
lsystemToArc :: [ColouredLine] -> [ColouredLine]
lsystemToArc [] = []
lsystemToArc [a] = []
lsystemToArc ((a, b, x):p@(c,d, _):ls)
  | colinear a b d = (m1, m2, x) : rest
  | otherwise = arcr ++ rest
  where
    m1 = midpoint a b
    m2 = midpoint c d
    rest = lsystemToArc (p:ls)
    arc = convertToArc m1 m2 b x
    fst3 (a, _, _) = a
    arcr = if radius (fst3 $ last arc) d < radius (fst3 $ head arc) d then arc else reverse arc

-- creates a gradient between the 2 given colours on the coloured lines
mapCol :: [ColouredLine] -> Colour -> Colour -> Int -> Int -> [ColouredLine]
mapCol [] x y k o = []
mapCol ((v1, v2, _):ls) x@(rx, gx, bx) y@(ry, gy, by) k o
  = (v1, v2, col) : mapCol ls x y k o
  where
    n = fromIntegral (mod (length ls + o) k)
    i = fromIntegral k
    r = rx + n*(ry-rx)/i
    g = gx + n*(gy-gx)/i
    b = bx + n*(by-bx)/i
    col = (r, g, b)

-- creates multiple gradients using one colour as a midpoint and the other an an
-- extreme value, the triple of ints is used to set the period of ther sine wave#
-- used for each of the r, g, b channels
mapColSine :: [ColouredLine] -> Colour -> Colour -> (Int, Int, Int) -> Int -> [ColouredLine]
mapColSine [] x y k o= []
mapColSine ((v1, v2, _):ls) x@(rx, gx, bx) y@(ry, gy, by) k@(dr, dg, db) o
  = (v1, v2, col) : mapColSine ls x y k o
  where
    n = length ls + o
    f d = fromIntegral (mod n d) / fromIntegral d
    r = rx + (ry-rx) * sin (2 * pi * f dr)
    g = gx + (gy-gx) * sin (2 * pi * f dg)
    b = bx + (by-bx) * sin (2 * pi * f db)
    col = (r, g, b)

rot :: Vertex -> Angle -> Vertex
rot (x, y) a = (x * cos r - y * sin r, x * sin r + y * cos r)
              where
                r = radians a


rotateLines :: [ColouredLine] -> Angle -> [ColouredLine]
rotateLines lines d = [(rot a d, rot b d, c) | (a, b, c) <- lines]

translateLines :: (Float, Float) -> [ColouredLine] -> [ColouredLine]
translateLines (dx, dy) lines = [((x1 + dx, y1 + dy), (x2 + dx, y2 + dy), c) | ((x1, y1), (x2, y2), c) <- lines]

-- draws an animated lsystem with colours mapped to it
drawAni = drawLinesA generateFrame


--generates a single frame of an animation
generateFrame frame = do
  let offset = 5 * frame
  return (mapColSine l (0.5, 0.5, 0.5) (1, 1, 1) (100, 200, 300) offset)
  where
    name = peanoGosper
    system = (angle name, axiom name, rules name)
    n = 3
    l = lsystemToArc (trace1 (expandLSystem system n) (angle system) (0, 0, 1))


drawAniTree = drawLinesA generateFrameTree
generateFrameTree frame = do
  let offset = 4 * frame
  return ((mapColSine bg (0.1, 0.1, 0.2) (0.2, 0.2, 0.2) (100, 600, 600) offset) ++ (mapColSine l (0.5, 0.5, 1) (1, 1, 1) (100, 600, 600) offset))
  where
    name = tree
    ang = angle name -25 + 68 * sin (radians $ fromIntegral (mod frame 360))
    system = (ang, axiom name, rules name)
    n = 4
    l0 = lsystemToArc (trace1 (expandLSystem system n) (angle system) (0, 0, 1))
    bg = translateLines (27, 7) (lsystemToArc (trace1 (expandLSystem peanoGosper 4) (angle peanoGosper) (0, 0, 1)) )
    l = (l0 ++ l60 ++ l120 ++ l180 ++ l240 ++ l300)
      where
        l60 = rotateLines l0 60
        l120 = rotateLines l0 120
        l180 = rotateLines l0 180
        l240 = rotateLines l0 240
        l300 = rotateLines l0 300
        
        

drawAniRA = drawLinesA generateFrameRA
generateFrameRA frame = do
  let offset = 5 * frame
  return (mapColSine l (0, 0.5, 0.5) (0, 1, 1) (100, 100, 600) offset)
  where
    name = rightAngle4
    ang = angle name +30 * sin (radians $ fromIntegral (mod frame 360))
    system = (ang, axiom name, rules name)
    n = 3
    l = lsystemToArc (trace1 (expandLSystem system n) (angle system) (0, 0, 1))


-- main function so this file can be compiled
main = do
  drawAniTree


----------------------------------------------------------
-- Some test systems.

-- credit for hilbert and tetraDragon - https://en.wikipedia.org/wiki/L-system
-- credit for rightAngle(1, 2, 3) - https://github.com/arendsee/lsystems

tetraDragon, hilbert, cross, triangle, arrowHead, peanoGosper, dragon, snowflake, tree, bush, canopy, galaxy :: LSystem

rightAngle1
  = (
    90,
    "F-F-F-F",
    [('+', "+"),
     ('-', "-"),
     ('F', "F+FF-FF-F-F+F+FF-F-F+F+FF+FF-F")]
  )

rightAngle2
  = (
    90,
    "F-F-F-F",
    [('+', "+"),
     ('-', "-"),
     ('F', "FF+F+F-FFF-FFF-FFF-F+F+FF")]
  )

rightAngle3
  = (
    90,
    "F-F-F-F",
    [('+', "+"),
     ('-', "-"),
     ('F', "FF-F-F+F+F-F+F+F-F+F+F-F-FF")]
  )

rightAngle4
  = (
    90,
    "F-F-F-F",
    [('+', "+"),
     ('-', "-"),
     ('F', "FF-F+F+F-F")]
  )

tetraDragon
  = (
    120,
    "N",
    [('+', "+"),
     ('-', "-"),
     ('N', "N+N-N")]
  )

hilbert
  = (90,
    "X",
    [('+', "+"),
     ('-', "-"),
     ('N', "N"),
     ('X', "+YN-XNX-NY+"),
     ('Y', "-XN+YNY+NX-")]
  )

cross
  = (90,
     "M-M-M-M",
     [('M', "M-M+M+MM-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

triangle
  = (90,
     "-M",
     [('M', "M+M-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

arrowHead
  = (60,
     "N",
     [('M', "N+M+N"),
      ('N', "M-N-M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

peanoGosper
  = (60,
     "M",
     [('M', "M+N++N-M--MM-N+"),
      ('N', "-M+NN++N+M--M-N"),
      ('+', "+"),
      ('-', "-")
     ]
    )

dragon
  = (45,
     "MX",
     [('M', "A"),
      ('X', "+MX--MY+"),
      ('Y', "-MX++MY-"),
      ('A', "A"),
      ('+', "+"),
      ('-', "-")
     ]
    )

snowflake
  = (60,
     "M--M--M",
     [('M', "M+M--M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

tree
  = (45,
     "M",
     [('M', "N[-M][+M][NM]"),
      ('N', "NN"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

bush
  = (22.5,
     "X",
     [('X', "M-[[X]+X]+M[+MX]-X"),
      ('M', "MM"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

canopy
  = (30.0,
     "M",
     [('M', "M[+MM][-MM]M[-M][+M]M"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

galaxy
  = (36.0,
     "[M]++[M]++[M]++[M]++[M]",
     [('M', "+M--M---M"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

commandMap :: Rules
commandMap
  = [('M', "F"),
     ('N', "F"),
     ('X', ""),
     ('Y', ""),
     ('A', ""),
     ('[', "["),
     (']', "]"),
     ('+', "L"),
     ('-', "R"),
     ('F', "F")
    ]


-- if you are reading this far then yes i did just copy the whole graphics.hs file
-- and paste it here so i could modify it
-- it was needed to make the animations work
-- technically this isnt against the rules as i havent modified graphics.hs and
-- everything works with the normal graphics.hs (as im not even importing it)

type Colour
  = (Float, Float, Float)

black, blue, green, cyan, red, magenta, yellow, white :: Colour

black   = (0, 0, 0)
blue    = (0, 0, 1.0)
green   = (0, 1.0, 0)
cyan    = (0, 1.0, 1.0)
red     = (1.0, 0, 0)
magenta = (1.0, 0, 1.0)
yellow  = (1.0, 1.0, 0)
white   = (1.0, 1.0, 1.0)

drawLines :: [ColouredLine] -> IO ()
drawLines ls
  = do
      () <- ls `seq` return ()
      _ <- getArgsAndInitialize
      w <- createWindow "LSystems"

      actionOnWindowClose   $= ContinueExecution
      displayCallback       $= display ls
      keyboardMouseCallback $= Just keyboardMouse
      reshapeCallback       $= Just reshape

      mainLoop

--frameCounter :: IORef GLint -> IdleCallback
frameCounter frame = do
  frame $~! ( + 1)
  postRedisplay Nothing

drawLinesA :: (Int -> IO [ColouredLine]) -> IO ()
drawLinesA ls
  = do
      --() <- ls `seq` return ()
      _ <- getArgsAndInitialize
      w <- createWindow "LSystems"
      frame <- newIORef 0
      
      idleCallback          $= Just ( frameCounter frame)
      actionOnWindowClose   $= ContinueExecution
      displayCallback       $= do
                                f <- get frame
                                lss <- ls f
                                display lss
      keyboardMouseCallback $= Just keyboardMouse
      reshapeCallback       $= Just reshape

      mainLoop

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

keyboardMouse :: Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse (Char '\ESC') Down _ _
  = leaveMainLoop

keyboardMouse (MouseButton _) Down _ p
  = print p

keyboardMouse _ _ _ _
  = return ()

reshape :: Size -> IO ()
reshape s
  = do
      viewport $= (Position 0 0, s)
      postRedisplay Nothing

computeScale :: [Vertex] -> (Vertex, Vertex)
computeScale vs
  = foldl' f ((infinity, infinity), (-infinity, -infinity)) vs
    where
      f ((minX, minY), (maxX, maxY)) (x, y)
        = ((min minX x, min minY y), (max maxX x, max maxY y))

      infinity
        = 10000000

