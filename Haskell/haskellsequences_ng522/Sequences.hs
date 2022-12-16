{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Sequences where

import Data.Char (ord, chr)

-- Extention functions that arent part of the problem sheet will all end in _EXT
-- for the functions that extend one of the original functions they will be tested against all the same test cases as the original function + some extra ones to test the new functionallity
-- From this commit onwards the TestSuite.hs file has been modified to allow for a small error in the output ( |error| < 0.0000001) to avoid issues with floating point error in the sequences and series functions
-- The functions that use this have their tests in the sequencesTestCasesWithError list in the Tests.hs file, all other tests are uneffected
-- (It seems that LabTS doesnt use my modified TestSuite.hs file so the tests for sequences are series arent run on LabTS, sorry for any inconveniece)

-- Returns the first argument if it is larger than the second,
-- the second argument otherwise
maxOf2 :: Int -> Int -> Int
maxOf2 a b
 | a > b = a
 | otherwise = b

-- Extention of maxOf2
-- Returnes the larger argument of a and b where a, b can be of any type that can be ordered
maxOf2_EXT :: (Ord a) => a -> a -> a
maxOf2_EXT a b
 | a > b = a
 | otherwise = b

-- Returns the largest of three Ints
maxOf3 :: Int -> Int -> Int -> Int
maxOf3 a b c
  = maxOf2 a (maxOf2 b c)

-- Extention of maxOf3
-- Returnes the larger argument of a, b and c where a, b, c can be of any type that can be ordered
maxOf3_EXT :: (Ord a) => a -> a -> a -> a
maxOf3_EXT a b c = maxOf2_EXT a (maxOf2_EXT b c)

-- Returns True if the character represents a digit '0'..'9';
-- False otherwise
isADigit :: Char -> Bool
isADigit d

  = d `elem` ['0'..'9']

-- Extention of isADigit
-- Takes a string and returns true if that string is a natural number
-- Checks if each individual character is a digit recursively, the empty string is considered to be equal to 0
isANatural_EXT :: String -> Bool
isANatural_EXT "" = True
isANatural_EXT (s:xs)
 = isADigit s && isANatural_EXT xs

-- Extention of isANatural
-- Takes a string and returns true if that string is a real number
-- checks if each individual character is a digit reccursively allowing for at most 1 decimal point in the number using the b flag
-- isAReal_EXT removes at most 1 - from the front of the number to account for - numbers and then passes the result to isAReal_EXT_

isAReal_EXT :: String -> Bool
isAReal_EXT "" = True
isAReal_EXT (s:xs)
 | s == '-' && not (null xs) = isAReal_EXT_ xs True --if it starts with a - remove it (except in the case where the whole string is "-" as that isnt a valid number)
 | otherwise = isAReal_EXT_ (s:xs) True

-- isAReal_EXT_ performs the digit/decimal point check on any remaining characters

isAReal_EXT_ :: String -> Bool -> Bool
isAReal_EXT_ "" b = True
isAReal_EXT_ (s:xs) b
 | s == '.' && b = isAReal_EXT_ xs False --case when the first . if found
 | s == '.' = False --case when more than 1 . is found
 | isADigit s = isAReal_EXT_ xs b
 | otherwise = False

-- Returns True if the character represents an alphabetic
-- character either in the range 'a'..'z' or in the range 'A'..'Z';
-- False otherwise
isAlpha :: Char -> Bool
isAlpha c
  = c `elem` (['a'..'z']++['A'..'Z'])

-- Extention of isAlpha
-- Takes in a string and returns true if that string only consists of alphabetic characters
isWord :: String -> Bool
isWord "" = True
isWord (s:xs) = isAlpha s && isWord xs

-- Returns the integer [0..9] corresponding to the given character.
-- Note: this is a simpler version of digitToInt in module Data.Char,
-- which does not assume the precondition.
digitToInt :: Char -> Int
-- Pre: the character is one of '0'..'9'
digitToInt d
  = ord d - ord '0'
  
-- Extention to digitToInt
-- Takes in a string and returns the integer value of that string (using digitToInt)

stringToInt_EXT :: String -> Int
-- Pre: each character is one of '0'..'9' except the first one which can be a '-'
stringToInt_EXT "" = 0
stringToInt_EXT (s:xs)
 | s == '-' = -1 * (stringToInt_EXT_ (reverse xs))
 | otherwise = stringToInt_EXT_ (reverse (s:xs))

stringToInt_EXT_ :: String -> Int
stringToInt_EXT_ "" = 0
stringToInt_EXT_ (x:xs) = digitToInt x + 10*(stringToInt_EXT_ xs)

-- Returns the upper case character corresponding to the input.
-- Uses guards by way of variety.
toUpper :: Char -> Char
toUpper c
  | ord c >= ord 'a' && ord c <= ord 'z' = ['A'..'Z']!!(ord c - ord 'a')
  | otherwise = c
  
-- Extention to toUpper
-- Takes a string and converts each character to upper case (ignores all non lowercase characters)
  
wordToUpper_EXT :: String -> String
wordToUpper_EXT "" = ""
wordToUpper_EXT (s:xs) = (toUpper s):(wordToUpper_EXT xs)

--
-- Sequences and series
--

-- Arithmetic sequence
arithmeticSeq :: Double -> Double -> Int -> Double
arithmeticSeq a d n
  = a + fromIntegral n*d

-- Arithmetic with recursion
-- Exactly the same as arithmeticSeq but withouyt using the explicit formula
arithmeticSeq_EXT :: Double -> Double -> Int -> Double
arithmeticSeq_EXT a d 0 = a
arithmeticSeq_EXT a d n = d + arithmeticSeq_EXT a d (n-1)

-- Geometric sequence
geometricSeq :: Double -> Double -> Int -> Double
geometricSeq a r n
  = a * (r ** fromIntegral n)

-- geometricSeq with recursion
-- Exactly the same as geometricSeq but without using the explicit formula
geometricSeq_EXT :: Double -> Double -> Int -> Double
geometricSeq_EXT a r 0 = a
geometricSeq_EXT a r n = r * geometricSeq_EXT a r (n-1)

-- Arithmetic series
arithmeticSeries :: Double -> Double -> Int -> Double
arithmeticSeries a d k
  = (n+1) * (a + d*n/2)
  where n = fromIntegral k

-- Extention of arithmeticSeries
-- Exactly the same as arithmeticSeries but without using the explicit formula defined in the problem statement

arithmeticSeries_EXT :: Double -> Double -> Int -> Double
arithmeticSeries_EXT a d 0 = a
arithmeticSeries_EXT a d n = arithmeticSeq_EXT a d n + arithmeticSeries_EXT a d (n-1)

-- Geometric series
geometricSeries :: Double -> Double -> Int -> Double
geometricSeries a r k
  | r == 1.0 = a * (n-1)
  | otherwise = a * (1 - r**(n+1))/(1-r)
  where n = fromIntegral k

-- Extention of geometricSeries
-- Exactly the same as geometricSeries but without using the explicit formula defined in the problem statement

geometricSeries_EXT :: Double -> Double -> Int -> Double
geometricSeries_EXT a r 0 = a
geometricSeries_EXT a r n = geometricSeq_EXT a r n + geometricSeries_EXT a r (n-1)

-- finding sum to infinity of geometric series
-- Finds the sum to infinity of a geometric series with |r| < 1 or how it diverges if |r| >= 1

-- A new type to account for the cases where the series converges, diverges to infinity/-infinity or when r <= -1 and it is alternating
data Converge a = Convergent a | Infinity | NegativeInfinity | Undefined deriving (Show, Eq)

-- function takes in the first term and common ratio of the series and outputs a Converge Double corresponding to what happens to the series
sumToInfinity_EXT :: Double -> Double -> Converge Double
sumToInfinity_EXT a r 
 | r >= 1 && a > 0 = Infinity
 | r >= 1 && a < 0 = NegativeInfinity
 | r <= -1 = Undefined
 | otherwise = Convergent (a/(1-r))
