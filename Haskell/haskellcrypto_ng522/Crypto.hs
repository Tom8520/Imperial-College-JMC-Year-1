{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Crypto where

import Data.Char

import Prelude hiding (gcd)

{-
The advantage of symmetric encryption schemes like AES is that they are efficient
and we can encrypt data of arbitrary size. The problem is how to share the key.
The flaw of the RSA is that it is slow and we can only encrypt data of size lower
than the RSA modulus n, usually around 1024 bits (64 bits for this exercise!).

We usually encrypt messages with a private encryption scheme like AES-256 with
a symmetric key k. The key k of fixed size 256 bits for example is then exchanged
via the aymmetric RSA.
-}

{-
The TestSuite.hs file has been altered again (this time to allow Integers as
inputs to tests instead of just Ints) so some tests wont work on LabTS as it
doesnt use my modified file.
Unlike the sequences task this one has a good range of test cases already for
most functions so adding more is pointless.
-}

-------------------------------------------------------------------------------

-- PART 1 : asymmetric encryption

gcd :: Int -> Int -> Int
gcd a b
 | b == 0    = a --check if base case has been met
 | otherwise = gcd b (mod a b) --if it hasnt then move on to the next iteration

-- Get the number of numbers less than n which are coprime to n
-- Use a list comprehension to check each valid number
phi :: Int -> Int
phi n = length [x | x <- [1..n], gcd x n == 1]

-- Calculates (u, v, d) the gcd (d) and Bezout coefficients (u and v)
-- such that au + bv = d
computeCoeffs :: Int -> Int -> (Int, Int)
--base case, if b = 0 then the equation becomes ua + 0v = 1
computeCoeffs a 0 = (1, 0) 
-- use the set of coefficients for the next a, b to construct the coefficients
computeCoeffs a b = (v', u'-q*v') 
               where
                --get the coefficients for the next iteration
                (u', v') = computeCoeffs b r
                (q, r)   = quotRem a b

-- Inverse of a modulo m
-- alternate implementation using that a^-1 = a^(phi(m)-1) (mod m)
-- with my implementations, using bezout coefficients has a time complexity 
-- of O(log m) but since phi is linear it will have a complexity of O(m)
inverse :: Int -> Int -> Int
inverse a m = mod (fst (computeCoeffs a m)) m

inverseAlternate :: Int -> Int -> Int
inverseAlternate a m = modPow a (phi m - 1) m

-- Calculates (a^k mod m)
-- if k is even then split into (a^2)^j
-- if k is odd split into a * (a^2)^j
modPow :: Int -> Int -> Int -> Int
modPow a 0 m = mod 1 m --base case of anything ^ 0 = 1
modPow a k m
 | even k    = modPow (mod (a*a) m) (div k 2) m 
 | otherwise = mod (a*modPow a (k-1) m) m 

-- Returns the smallest integer that is coprime with phi
smallestCoPrimeOf :: Int -> Int
smallestCoPrimeOf n = check n 2--start checking at 2 since gcd(x, 1) is always 1
 where check a b
        | gcd a b == 1 = b -- if a and b are coprime then return b
        | otherwise    = check a (b+1) -- otherwise move on to the next b

-- Generates keys pairs (public, private) = ((e, n), (d, n))
-- given two "large" distinct primes, p and q
genKeys :: Int -> Int -> ((Int, Int), (Int, Int))
genKeys p q = ((e, n), (d, n))
              where
                n = p*q
                m = (p-1)*(q-1)
                e = smallestCoPrimeOf m
                d = inverse e m

-- RSA encryption/decryption
rsaEncrypt :: Int -> (Int, Int) -> Int
rsaEncrypt x (e, n) = modPow x e n

rsaDecrypt :: Int -> (Int, Int) -> Int
rsaDecrypt c (d, n) = modPow c d n

-- RSA Text encryption/decryption
-- The first few functions here are copies of pervious fuctions but made to work
-- with larger numbers, so comments have been excluded and they arent tested as
-- the comments/test results would be identical

inverseLarge :: Integer -> Integer -> Integer
inverseLarge a m = mod (fst (computeCoeffsLarge a m)) m

computeCoeffsLarge :: Integer -> Integer -> (Integer, Integer)
computeCoeffsLarge a 0 = (1, 0) 
computeCoeffsLarge a b = (v', u'-q*v') 
               where
                (u', v') = computeCoeffsLarge b r
                (q, r)   = quotRem a b

gcdLarge :: Integer -> Integer -> Integer
gcdLarge a b
 | b == 0    = a
 | otherwise = gcdLarge b (mod a b)

smallestCoPrimeOfLarge :: Integer -> Integer
smallestCoPrimeOfLarge n = check n 2
                          where
                            check n k
                             | gcdLarge n k == 1 = k
                             | otherwise         = check n (k+1)

genKeysLarge :: Integer -> Integer -> ((Integer, Integer), (Integer, Integer))
genKeysLarge p q = ((e, n), (d, n))
              where
                n = p*q
                m = (p-1)*(q-1)
                e = smallestCoPrimeOfLarge m
                d = inverseLarge e m

modPowAny :: Integral a => a -> a -> a -> a
modPowAny a 0 m = mod 1 m
modPowAny a k m
 | even k    = modPowAny (mod (a*a) m) (div k 2) m
 | otherwise = mod (a*modPowAny a (k-1) m) m

-- stoi (string to integer)
-- takes in a string and converts each char to a 3 digit ascii code 
stoi :: String -> Integer
stoi "" = 0
stoi s = (1000 * stoi (init s)) + toInteger (ord (last s))

-- intToString
-- opposite of stoi, takes a string and converts each 3 digit block (starting
-- from the least significant digit) to its char
intToString :: Integer -> String
intToString 0 = ""
intToString n = intToString (div n 1000) ++ [chr (fromIntegral (mod n 1000))]

-- rsaEncryptText
-- applies the RSA algorithm to the string after converting to an integer
-- the result is then converted back to a string
rsaEncryptText :: String -> (Integer, Integer) -> String
rsaEncryptText s (e, n) = intToString ( modPowAny (stoi s) e n)

-- rsaDecryptText
-- opposite of rsaEncryptText, decrypts a string obtained from rsaEncryptText
rsaDecryptText :: String -> (Integer, Integer) -> String
rsaDecryptText s (d, n) = intToString ( modPowAny (stoi s) d n)

-------------------------------------------------------------------------------
-- PART 2 : symmetric encryption

-- Returns position of a letter in the alphabet
toInt :: Char -> Int
toInt a = ord a - ord 'a'

-- Returns the n^th letter
toChar :: Int -> Char
toChar a = chr ( a + ord 'a')

-- "adds" two letters
add :: Char -> Char -> Char
add a b = toChar (mod (toInt a + toInt b) 26)

-- "substracts" two letters
substract :: Char -> Char -> Char
substract a b = toChar (mod (toInt a - toInt b) 26)

-- the next functions present`

-- 2 modes of operation for block ciphers : ECB and CBC
-- based on a symmetric encryption function e/d such as "add"

-- ecb (electronic codebook) with block size of a letter
--
ecbEncrypt :: Char -> String -> String
ecbEncrypt k s = [add x k | x <- s]

ecbDecrypt :: Char -> String -> String
ecbDecrypt k s = [substract x k | x <- s]

-- cbc (cipherblock chaining) encryption with block size of a letter
-- initialisation vector iv is a letter
-- last argument is message m as a string
--
cbcEncrypt :: Char -> Char -> String -> String
cbcEncrypt _ _ []     = [] --base case when there are no more characters left
cbcEncrypt k v (x:xs) = c : cbcEncrypt k c xs
                        where
                          -- helper function for clarity, could be removed
                          encrypt a b = add (add a b) k
                          c = encrypt x v

cbcDecrypt :: Char -> Char -> String -> String
cbcDecrypt _ _ []     = [] --base case when there are no more characters left
cbcDecrypt k v (c:cs) = x : cbcDecrypt k c cs
                        where
                          -- helper function for clarity, could be removed
                          -- b could also be removed from both sides but i have
                          -- left it in for clarity
                          decrypt a b = substract (substract a k) b
                          x = decrypt c v
