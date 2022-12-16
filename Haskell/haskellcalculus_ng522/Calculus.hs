module Calculus where

import Data.Maybe
import IC.Graphics hiding (Colour)

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord)

-- type for predicates, can either be == && || ¬ < > <= >=
-- used for adding piecewise functions
data Pred = Equ Exp Exp | And Pred Pred | Or Pred Pred | Not Pred
          | Less Exp Exp | More Exp Exp | LessEq Exp Exp | MoreEq Exp Exp
          deriving (Eq, Ord)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         | If Pred Exp Exp
         deriving (Eq, Ord)

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

class Vars a where
  x, y, z :: a

instance Vars Exp where
  x = Id "x"
  y = Id "y"
  z = Id "z"

instance Vars Double where
  x = 4.3
  y = 9.2
  z = -1.7

instance Num Exp where
  fromInteger = Val . fromInteger
  negate      = UnApp Neg
  (+)         = BinApp Add
  (*)         = BinApp Mul
-- Leave the following two undefined...
  signum      = undefined
  abs         = undefined

instance Fractional Exp where
  fromRational = Val . fromRational
  (/)          = BinApp Div
-- Leave the following one undefined...
  recip        = BinApp Div (Val 1)

instance Floating Exp where
  sin     = UnApp Sin
  cos     = UnApp Cos
  log     = UnApp Log
-- Leave the following fifteen undefined...
  tan     = undefined
  asin    = undefined
  acos    = undefined
  atan    = undefined
  pi      = undefined
  exp     = undefined
  sqrt    = undefined
  (**)    = undefined
  logBase = undefined
  sinh    = undefined
  cosh    = undefined
  tanh    = undefined
  asinh   = undefined
  acosh   = undefined
  atanh   = undefined

---------------------------------------------------------------------------

-- look up a given key in a list of key, val pairs and return the corresponding
-- val if it exists, oterwise return nothing
lookUp :: Eq a => a -> [(a, b)] -> Maybe b
lookUp a [] = Nothing
lookUp a ((x, y):xs)
  | a == x    = Just y
  | otherwise = lookUp a xs

instance Show UnOp where
  show Neg = "-"
  show Sin = "sin "
  show Cos = "cos "
  show Log = "ln "

instance Show BinOp where
  show Add = " + "
  show Mul = " * "
  show Div = " / "

instance Show Exp where
  show (Val a) = show a
  show (Id a) = a
  show (UnApp op a) = show op ++ "(" ++ show a ++ ")"
  show (BinApp op a b) = "(" ++ show a ++ show op ++ show b ++ ")"

showExp :: Exp -> String
showExp
  = show

-- map from the unary operator constructors to their functions
unOps = [
  (Neg, negate),
  (Sin, sin),
  (Cos, cos),
  (Log, log)
  ]

-- map from the binary operators to their functions
binOps = [
  (Add, (+)),
  (Mul, (*)),
  (Div, (/))
  ]

-- evaluate a predicate
evalPred :: Pred -> Env -> Bool
evalPred (Equ a b)    env = eval a env == eval b env
evalPred (And a b)    env = evalPred a env && evalPred b env
evalPred (Or a b)     env = evalPred a env || evalPred b env
evalPred (Not a)      env = not (evalPred a env)
evalPred (Less a b)   env = eval a env < eval b env
evalPred (More a b)   env = eval a env > eval b env
evalPred (LessEq a b) env = eval a env <= eval b env
evalPred (MoreEq a b) env = eval a env >= eval b env

-- takes in an expression and a list of variables and their values and evaluates
-- the expression with those values
eval :: Exp -> Env -> Double
eval (Val a) env = a
eval (Id a) env = fromJust (lookUp a env)
eval (UnApp op a) env = fromJust (lookUp op unOps) (eval a env)
eval (BinApp op a b) env = fromJust (lookUp op binOps) (eval a env) (eval b env)
eval (If p a b) env = if evalPred p env then eval a env else eval b env


-- helper for diff, implements the differantiation rules for all of the unary
-- operations
-- takes in both the expression and its derivative ( a and a' )
diffUnOp :: UnOp -> Exp -> Exp -> Exp
diffUnOp Neg a a' = -a'
diffUnOp Sin a a' = cos a * a'
diffUnOp Cos a a' = - (sin a * a')
diffUnOp Log a a' = a' / a

-- helper for diff, implements the differentiation rules for all of the binary
-- operations
diffBinOp :: BinOp -> Exp -> Exp -> Exp -> Exp -> Exp
diffBinOp Add a b a' b' = a' + b'
diffBinOp Mul a b a' b' = (a * b') + (a' * b)
diffBinOp Div a b a' b'
  = num / den
  where
    num = (a' * b) + (-(a * b'))
    den = b * b

-- takes in an expression and returnes the derivative of the expression wrt the
-- given variable
diff :: Exp -> String -> Exp
diff (Val _) var = Val 0
diff (Id a) var
  | a == var  = Val 1
  | otherwise = Val 0
diff (UnApp op a) var 
  = diffUnOp op a a'
  where
    a' = diff a var
diff (BinApp op a b) var 
  = diffBinOp op a b a' b'
  where
    a' = diff a var
    b' = diff b var
diff (If p a b) var
  = If p a' b'
  where
    a' = diff a var
    b' = diff b var


-- takes in a function ( in the form of an expression ), a value we want to
-- approximate the function at and the number of terms of the series we want
-- to uses
maclaurin :: Exp -> Double -> Int -> Double
maclaurin f a n
  = sum terms
  where
    der    = iterate (`diff` "x") f
    fact   = scanl (*) 1 [1..(fromIntegral n-1)]
    pow    = scanl (*) 1 (repeat a)
    numDer = [eval x [("x", 0)] | x <- der]

    terms  = zipWith3 (\x y z -> x * z/y) numDer fact pow
---------------------------------------------------------------------------
-- extentions
-- visualisation of taylor series

-- clicking in the window will cause the taylor series to become centered at
-- your mouses x coordinate
-- pressing the 'd' key will increase the number of terms used in the expansion
-- pressing the 'a' key will decrease the number of terms used in the expansion

-- same as maclaurin but can be centered at any value
taylor :: Exp -> Double -> Double -> Int -> Double
taylor f a c n 
  = sum terms
    where
      der    = iterate (`diff` "x") f
      fact   = scanl (*) 1 [1..(fromIntegral n-1)]
      pow    = scanl (*) 1 (repeat (a-c))
      numDer = [eval x [("x", c)] | x <- der]

      terms  = zipWith3 (\x y z -> x * z/y) numDer fact pow

type Vertex = (Double, Double)

type Colour = (Float, Float, Float)

type ColouredLine = (Vertex, Vertex, Colour)

-- take in a function f(x) and generate a set of coloured lines to approximate
-- the function bettween a given start and end point, the Int is the number
-- of lines to generate
generateGraph :: ( Double -> Double ) -> Double -> Double -> Int -> Colour -> [ColouredLine]
generateGraph f min max r col
  = lines
  where
    r' = fromIntegral r
    h = (max - min)/r'
    points = [(min + fromIntegral i*h,f (min + fromIntegral i*h)) | i <- [0..r]]
    lines = [(points!!i, points!!(i+1), col) | i <- [0..(r-1)]]

-- Converts an expression into a function that can be evaluated without calling
-- eval
expToFun :: Exp -> (Double -> Double)
expToFun exp x = eval exp [("x", x)]

-- converts an expression into a function that approximates it using a
-- taylor expansion
generateTaylorFun :: Exp -> Double -> Int -> Double -> Double
generateTaylorFun exp c n x = taylor exp x c n

-- takes in a list of lines which represents 1 (or more) functions and removes
-- all of the ones which are outside the vertical range of the viewport
filterY :: [ColouredLine] -> Double -> Double -> [ColouredLine]
filterY lines min max 
  = filter f lines
  where
    f ((_, a), (_, b), _) = a >= min && a <= max && b >= min && b <= max

-- takes in a double which is known to be between 2 values and maps it to be
-- between 2 other values
mapDouble :: Double -> Double -> Double -> Double -> Double -> Double
mapDouble x a b a' b' = (x-a)/(b-a) * (b' - a') + a'


-- draws a function, the coordinate axes and the functions taylor series
-- with parameters that are passe from within the graphics.hs file for
-- the number or terms, center value for the series and width of the window
drawSeries :: Int -> Int -> Double -> Double -> [ColouredLine]
drawSeries frame n c w
  = graphs ++ axis
  where
    exp = sin ( Id "x")
    f   = expToFun exp
    min = -4.0
    max = 4.0
    r   = 100
    g   = generateGraph f min max r blue
    
    c'  = mapDouble c 0 w min max
    f'  = generateTaylorFun exp c' n
    g'  = generateGraph f' min max r red

    graphs = filterY (g++g') min max

    axis = drawAxis min max min max

-- generates 2 lines, one for each of the coordinate axis
drawAxis :: Double -> Double -> Double -> Double -> [ColouredLine]
drawAxis minX maxX minY maxY
  = [xAxis, yAxis]
  where
    xAxis = ((minX, 0), (maxX, 0), white)
    yAxis = ((0, minY), (0, maxY), white)

-- main function to allow the visulaisation to be displayed
main :: IO ()
main = do
  drawLinesA drawSeries
    
---------------------------------------------------------------------------
-- Test cases...

e1, e2, e3, e4, e5, e6 :: Exp

-- 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- x*x+y-7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))
