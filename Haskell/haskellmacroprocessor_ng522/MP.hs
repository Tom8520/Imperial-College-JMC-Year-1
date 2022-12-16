module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"

{-
Once again, TestSuite.hs has been edited. This time to allow for files to be
directly passed as inputs instaed of copying the contents into Tests.hs.
Idk how all of the error handling works so i havent included it so if you try
and use it with code that produces an error idk what will happen. So some tests
might not work on LabTS as a result of this.
-}

-----------------------------------------------------

-- takes in a string and a list of (key, val) pairs and returns a list of all
-- the vals which have the given key
lookUp :: String -> [(String, a)] -> [a]
lookUp key list
  = [b | (a, b) <- list, a == key]

-- takes a list of seperators and a string and splits the string wherever there
-- is a seperator while maintaining a list of all the seperators that appear
splitText :: [Char] -> String -> (String, [String])
splitText seps "" = ("", [""])
splitText seps (x:xs)
  | x `elem` seps = (x:a, "":b:bs)
  | otherwise     = (a  , (x:b):bs)
  where
    (a, b:bs) = splitText seps xs
    
-- almost the opposite of splitText, takes a list of seperators and a list of
-- the words they go between and returns a list of words with the spaces
-- inserted in the correct positions (concat to fully reverse splitText)
combine :: String -> [String] -> [String]
combine "" s = s
combine seps [] = [[c] | c <- seps]
combine (sep:seps) (s:xs)
  = s:[sep]:combine seps xs

-- takes in a list of lines and retuens a list of key, val pairs where each key
-- starts with $
getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = []
getKeywordDefs (l:ls)
  = (key, val):getKeywordDefs ls
  where
    (sep:seps, x:xs) = splitText " " l
    key = x
    val = concat (combine seps xs)

-- reads a file containing some text and a file containing some key, val pairs
-- and replaces every occurance of a key in the text with its corresponding val
expand :: FileContents -> FileContents -> FileContents
expand text info
  = concat ( combine seps [replaceWord word defs | word <- words])
  where
    isKeyValPair s 
     | null s    = False
     | otherwise = head s == '$'
     
    (seps, words) = splitText separators text
    defs = getKeywordDefs (filter isKeyValPair (snd (splitText "\n" info)))

-- You may wish to uncomment and implement this helper function
-- when implementing expand
replaceWord :: String -> KeywordDefs -> String
replaceWord s defs
 | null vals = s
 | otherwise = head vals
 where vals = lookUp s defs

-- takes a file with multiple sets of definitions and produces an output for
-- eeach one seperated bt -----

enhancedExpand :: FileContents -> FileContents -> FileContents
enhancedExpand text info
  = concat [expand text d ++ "-----\n" | d <- dat]
  where
    dat = snd (splitText "#" info)

-- allows for recursive efinitions
-- for example if $name = "Tom $lastname" and $lastname = "Grange"
-- then expanding "My name is $name" would produce "My name is Tom Grange"

recursiveExpand :: FileContents -> FileContents -> FileContents
recursiveExpand text info
 | '$' `elem` text = recursiveExpand (expand text info) info
 | otherwise       = text
-----------------------------------------------------

-- The provided main program which uses your functions to merge a
-- template and source file.
main :: IO ()
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (enhancedExpand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")
