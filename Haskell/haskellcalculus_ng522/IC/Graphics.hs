module IC.Graphics
  ( Colour
  , black
  , blue
  , green
  , cyan
  , red
  , magenta
  , yellow
  , white
  , drawLinesA
  ) where

import Control.Monad
import Data.Char
import Data.List
--import Graphics.Rendering.OpenGL hiding (Vertex) -- kgk: Uncomment this line if you encounter any issues
import Graphics.UI.GLUT hiding (Vertex)
import Data.IORef (newIORef)

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

type Vertex
  = (Double, Double)

type ColouredLine
  = (Vertex, Vertex, Colour)

--frameCounter :: IORef GLint -> IdleCallback
frameCounter frame = do
  frame $~! ( + 1)
  postRedisplay Nothing

drawLinesA :: (Int -> Int -> Double -> Double -> [ColouredLine]) -> IO ()
drawLinesA genFrame
  = do
      --() <- ls `seq` return ()
      _ <- getArgsAndInitialize
      w <- createWindow "Taylor Series"
      frame <- newIORef 0
      center <- newIORef (0::Int)
      terms <- newIORef (0::Int)
      width <- newIORef (300::Int)
      
      -- idleCallback          $= Just ( frameCounter frame)
      actionOnWindowClose   $= ContinueExecution
      displayCallback       $= do
                                f <- get frame
                                c <- get center
                                t <- get terms
                                w <- get width
                                let d = genFrame f t (fromIntegral c) (fromIntegral w)
                                display d
      keyboardMouseCallback $= Just (keyboardMouse [terms,center])
      reshapeCallback       $= Just (reshape width)

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

keyboardMouse :: (HasUpdate t b b, Num b) =>  [t] -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse _ (Char '\ESC') Down _ _ 
  = leaveMainLoop

keyboardMouse (x:xs) (Char 'd') Down _ _ 
  = do
    x $~! (+1)
    postRedisplay Nothing

keyboardMouse (x:xs) (Char 'a') Down _ _ 
  = do
    x $~! flip (-) 1
    postRedisplay Nothing

keyboardMouse (_:x:xs) (MouseButton _) Down _ (Position x' y')
  = do
    x $~! const (fromIntegral x')
    postRedisplay Nothing

keyboardMouse _ _ _ _ _ 
  = return ()

reshape :: (HasUpdate t b b, Num b) => t -> Size -> IO ()
reshape w s@(Size x y)
  = do
      w $~! const (fromIntegral x)
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
