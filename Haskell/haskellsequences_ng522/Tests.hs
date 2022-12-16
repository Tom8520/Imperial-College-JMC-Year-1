module Tests where

import IC.TestSuite

import Sequences


maxOf2TestCases
  = [ (1, 2) ==> 2,
      (2, 1) ==> 2,
      (2, 2) ==> 2,
      (0, 0) ==> 0,
      (-3, -4) ==> (-3),
      (-2, 5) ==> 5
    ]


maxOf2_EXT_TestCases_Int :: [((Int, Int), Int)]
maxOf2_EXT_TestCases_Int
  = [ (1, 2) ==> 2,
      (2, 1) ==> 2,
      (2, 2) ==> 2,
      (0, 0) ==> 0,
      (-3, -4) ==> (-3),
      (-2, 5) ==> 5
    ]

maxOf2_EXT_TestCases_Float :: [((Float, Float), Float)]
maxOf2_EXT_TestCases_Float
  = [ (1.2, 2.1) ==> 2.1,
      (2.1, 1.2) ==> 2.1,
      (0.0, 0.0) ==> 0.0,
      (-5.6, -2.0) ==> (-2.0),
      (-2.5, 1.8) ==> 1.8
    ]

maxOf2_EXT_TestCases_Char :: [((Char, Char), Char)]
maxOf2_EXT_TestCases_Char
  = [ ('a', 'b') ==> 'b',
      ('b', 'a') ==> 'b',
      ('a', 'a') ==> 'a',
      ('A', 'a') ==> 'a',
      ('1', 'a') ==> 'a',
      ('+', '~') ==> '~'
    ]

maxOf3TestCases
  = [ (1, 2, 3) ==> 3,
      (2, 1, 3) ==> 3,
      (3, 3, 3) ==> 3,
      (-1, 0, 1) ==> 1,
      (-3, -2, -1) ==> (-1)
    ]

maxOf3_EXT_TestCases_Int :: [((Int, Int, Int), Int)]
maxOf3_EXT_TestCases_Int
  = [ (1, 2, 0) ==> 2,
      (2, 1, 0) ==> 2,
      (2, 2, 2) ==> 2,
      (0, 0, 0) ==> 0,
      (-3, -4, -5) ==> (-3),
      (-2, 5, 0) ==> 5
    ]

maxOf3_EXT_TestCases_Float :: [((Float, Float, Float), Float)]
maxOf3_EXT_TestCases_Float
  = [ (1.2, 2.1, 0.3) ==> 2.1,
      (2.1, 1.2, 0.3) ==> 2.1,
      (0.0, 0.0, 0.0) ==> 0.0,
      (-5.6, -2.0, -9.8) ==> (-2.0),
      (-2.5, 1.8, 0.3) ==> 1.8
    ]

maxOf3_EXT_TestCases_Char :: [((Char, Char, Char), Char)]
maxOf3_EXT_TestCases_Char
  = [ ('a', 'b', 'b') ==> 'b',
      ('b', 'a', 'b') ==> 'b',
      ('a', 'a', 'a') ==> 'a',
      ('A', 'a', 'Z') ==> 'a',
      ('1', 'a', '9') ==> 'a',
      ('+', '~', ',') ==> '~'
    ]

isADigitTestCases
  = [ ('1') ==> True,
      ('A') ==> False,
      ('0') ==> True,
      ('9') ==> True,
      ('g') ==> False,
      ('-') ==> False,
      (' ') ==> False,
      ('\0') ==> False
    ]

isANaturalTestCases
 = [ ("12345") ==> True,
     ("-1") ==> False,
     ("1.5") ==> False,
     ("0") ==> True,
     (" ") ==> False,
     ("") ==> True
 ]

isARealTestCases
  = [ ("12345") ==> True,
      ("-1.2345") ==> True,
      ("") ==> True,
      (" ") ==> False,
      ("-") ==> False,
      ("-345.") ==> True,
      (".") ==> True,
      ("1.2.3") ==> False,
      ("--1.2") ==> False
  ]

isAlphaTestCases
  = [ ('1') ==> False,
      ('A') ==> True,
      ('Z') ==> True,
      ('a') ==> True,
      ('z') ==> True,
      (']') ==> False,
      (' ') ==> False,
      ('\n') ==> False
    ]

isWordTestCases
 = [ ("Hello") ==> True,
     ("100") ==> False,
     ("") ==> True,
     (" ") ==> False,
     ("Hello World") ==> False,
     ("HelloWorld") ==> True,
     ("HelloWorld!") ==> False
 ]

digitToIntTestCases
  = [ ('0') ==> 0,
      ('5') ==> 5,
      ('9') ==> 9
    ]

stringToIntTestCases
 = [ ("123") ==> 123,
     ("0") ==> 0,
     ("-5") ==> (-5),
     ("0004") ==> 4
   ]

toUpperTestCases
  = [ ('a') ==> 'A',
      ('A') ==> 'A',
      ('Z') ==> 'Z',
      ('z') ==> 'Z',
      ('t') ==> 'T'
    ]

wordToUpperTestCases
 = [ ("Hello WoRlD!") ==> "HELLO WORLD!",
     ("HHHHH") ==> "HHHHH",
     ("test") ==> "TEST",
     ("") ==> "",
     (" ") ==> " ",
     ("idk\n") ==> "IDK\n"
   ]

--
-- Sequences and series
--

arithmeticSeqTestCases
  = [ (0.0, 10.0, 0) ==> 0.0,
      (10.0, 10.0, 0) ==> 10.0,
      (0.0, 10.0, 10) ==> 100.0,
      (10.0, 0.0, 10) ==> 10.0,
      (0.0, 0.0, 0) ==> 0.0,
      (5.0, -3.0, 13) ==> (-34.0),
      (1.2, 2.7, 9) ==> 25.5
    ]

geometricSeqTestCases
  = [ (0.0, 10.0, 0) ==> 0.0,
      (10.0, 10.0, 0) ==> 10.0,
      (0.0, 10.0, 10) ==> 0.0,
      (10.0, 0.0, 10) ==> 0.0,
      (32.0, 0.5, 4) ==> 2.0,
      (5.0, 1.5, 3) ==> 16.875
    ]

arithmeticSeriesTestCases
  = [ (0.0, 10.0, 0) ==> 0.0,
      (10.0, 10.0, 0) ==> 10.0,
      (0.0, 10.0, 10) ==> 550.0,
      (10.0, 0.0, 10) ==> 110.0,
      (0.0, 0.0, 0) ==> 0.0,
      (1.5, -2.5, 100) ==> (-12473.5)
    ]

geometricSeriesTestCases
  = [ (0.0, 10.0, 0) ==> 0.0,
      (10.0, 10.0, 0) ==> 10.0,
      (0.0, 10.0, 10) ==> 0.0,
      (10.0, 0.0, 10) ==> 10.0,
      (0.0, 0.0, 0) ==> 0.0,
      (5.5, 2.0, 15) ==> 360442.5,
      (64.0, -0.5, 8) ==> 42.75
    ]

sumToInfinityTestCases
 = [
  (0.0, 0.0) ==> Convergent 0.0,
  (2.0, 0.5) ==> Convergent 4.0,
  (-5.0, -0.6) ==> Convergent (-3.125),
  (1.0, 1.0) ==> Infinity,
  (-1.0, 1.0) ==> NegativeInfinity,
  (10.0, -1.5) ==> Undefined,
  (2.0, -5.0) ==> Undefined,
  (-2.0, 5.0) ==> NegativeInfinity
 ]

-- You can add your own test cases above

sequencesTestCases
  = [ TestCase  "maxOf2"      (uncurry maxOf2)
                              maxOf2TestCases
     , TestCase "maxOf2_EXT - Ints" (uncurry maxOf2_EXT)
                                    maxOf2_EXT_TestCases_Int
     , TestCase "maxOf2_EXT - Floats" (uncurry maxOf2_EXT)
                                    maxOf2_EXT_TestCases_Float
     , TestCase "maxOf2_EXT - Chars" (uncurry maxOf2_EXT)
                                    maxOf2_EXT_TestCases_Char
     , TestCase "maxOf3"      (uncurry3 maxOf3)
                              maxOf3TestCases
     , TestCase "maxOf3_EXT - Ints" (uncurry3 maxOf3_EXT)
                                    maxOf3_EXT_TestCases_Int
     , TestCase "maxOf3_EXT - Floats" (uncurry3 maxOf3_EXT)
                                    maxOf3_EXT_TestCases_Float
     , TestCase "maxOf3_EXT - Chars" (uncurry3 maxOf3_EXT)
                                    maxOf3_EXT_TestCases_Char
     , TestCase "isADigit"    (isADigit)
                              isADigitTestCases
     , TestCase "isANatural"    (isANatural_EXT)
                              isANaturalTestCases
     , TestCase "isAReal"    (isAReal_EXT)
                              isARealTestCases
     , TestCase "isAlpha"     (isAlpha)
                              isAlphaTestCases
     , TestCase "isWord"     (isWord)
                              isWordTestCases
     , TestCase "digitToInt"  (digitToInt)
                              digitToIntTestCases
     , TestCase "stringToInt"  (stringToInt_EXT)
                              stringToIntTestCases
     , TestCase "toUpper"     (toUpper)
                              toUpperTestCases
     , TestCase "wordToUpper"     (wordToUpper_EXT)
                              wordToUpperTestCases
     , TestCase "sumToInfinity"   (uncurry sumToInfinity_EXT)
                                    sumToInfinityTestCases
  ]
sequencesTestCasesWithError = [
       TestCaseWithError "arithmeticSeq" (uncurry3 arithmeticSeq)
                                arithmeticSeqTestCases
     , TestCaseWithError "arithmeticSeq without formula" (uncurry3 arithmeticSeq_EXT)
                                arithmeticSeqTestCases
     , TestCaseWithError "geometricSeq"  (uncurry3 geometricSeq)
                                geometricSeqTestCases
     , TestCaseWithError "geometricSeq without formula"  (uncurry3 geometricSeq_EXT)
                                geometricSeqTestCases
     , TestCaseWithError "arithmeticSeries"  (uncurry3 arithmeticSeries)
                                    arithmeticSeriesTestCases
     , TestCaseWithError "arithmeticSeries without formula"  (uncurry3 arithmeticSeries_EXT)
                                    arithmeticSeriesTestCases
     , TestCaseWithError "geometricSeries"   (uncurry3 geometricSeries)
                                    geometricSeriesTestCases
     , TestCaseWithError "geometricSeries without formula"   (uncurry3 geometricSeries_EXT)
                                    geometricSeriesTestCases
    ]

runTests = do
  mapM_ goTest sequencesTestCases
  mapM_ goTestWithError sequencesTestCasesWithError

main = runTests