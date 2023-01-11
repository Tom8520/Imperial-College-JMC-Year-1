import Prelude hiding (Word, lookup)
import Data.List hiding (lookup, delete)
import Data.Maybe

type Dict = [String]
data Clue = Words [String] | And Clue Clue | Synonym Clue | Anagram Clue | Reversal Clue | Insertion Clue Clue | Charade Clue Clue | Length Int Clue
            deriving (Show)
synonyms :: [[String]]
synonyms = [
             ["pardon", "forgive", "condone", "amnesty"],
             ["unsuitable", "inapt"],
             ["home", "in"],
             ["carol", "sing"],
             ["county", "kent"],
             ["palace", "kensington"],
             ["working", "on"],
             ["tap"]
           ]

lookup :: Eq a => [[a]] -> a -> [a]
lookup xss x = concat [xs | xs <- xss, x `elem` xs]

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False

perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [y : ys |y <- xs, ys <- perms (xs \\ [y]) ]

insertions :: [a] -> [a] -> [[a]]
insertions xs ys
  = [f ++ xs ++ l | (f, l) <- splits]
  where
    splits = [splitAt n ys | n <- [0..length ys]]

evalClue :: Clue -> [String]
evalClue (Words ws) 
  = ws
evalClue (Length n c)
  = [w | w <- evalClue c, length w == n]
evalClue (And c c')
  = ws `intersect` ws' 
  where
    ws = evalClue c
    ws' = evalClue c'
evalClue (Synonym c) 
  = concatMap (lookup synonyms) (evalClue c)
evalClue (Anagram c)
  = concatMap perms (evalClue c)
evalClue (Reversal c)
  = map reverse (evalClue c)
evalClue (Insertion c c') 
  = concatMap (uncurry insertions) combs
  where
    ws = evalClue c
    ws' = evalClue c'
    combs = [(w, w') | w <- ws, w' <- ws']
evalClue (Charade c c')
  = concat [[w ++ w', w'++w] | w <- ws, w' <- ws']
  where
    ws  = evalClue c
    ws' = evalClue c'

delete :: Eq a => [a] -> [a] -> [a]
delete s cs = [c | c <- s, c `notElem` cs]


anagramIdent = ["ornate"]
insertionIdent = ["in"]
reversalIdent = []

extractClue :: String -> [Clue]
extractClue s
  = convert ws''
  where
    ws  = delete (words $ delete s ",") ["for", "at"]
    ws' = [if w `elem` anagramIdent then "ANAGRAM" else w | w <- ws]
    ws'' = [if w `elem` insertionIdent then "INSERT" else w | w <- ws]
    ws''' = [if w `elem` reversalIdent then "REVERSE" else w | w <- ws]

    convert :: [String] -> [Clue]
    convert [] = []
    convert [w] = [Synonym (Words [w])]
    convert ("ANAGRAM":w:ws)
      = Anagram (Synonym (Words [w])) : convert ws
    convert ("REVERSE":w:ws)
      = Reversal (Synonym (Words [w])) : convert ws
    convert [w, w'] = convert [w] ++ convert [w']
    convert (w:"INSERT":w':ws)
      = Insertion (Synonym (Words [w])) (Synonym (Words [w'])) : convert ws
    convert (w:ws) = Synonym (Words [w]) : convert ws

addCharades :: Int -> [Clue] -> [Clue]
addCharades _ [] = []
addCharades _ [c] = [c]
addCharades n (c:c':cs)
  | even n    = addCharades n' $ Charade c c' : cs
  | otherwise = c : addCharades n' (c':cs)
  where
    n' = n `div` 2

combClues :: [Clue] -> Clue
combClues [c] = c
combClues (c:cs) = foldl And c cs

solveClue :: String -> String
solveClue s
  = head [head (evalClue(combClues (addCharades i cs))) | i <- [0..p-1], isSingleton (evalClue(combClues (addCharades i cs)))] 
  where
    (s', _:ls) = break (=='(') s
    l = read (init ls) :: Int

    cs = extractClue s'

    n = length cs - 1
    p = 2^n
    
c1 = "unsuitable at home, ornate tap (5)"
c2 = "carol, in county, working for palace (10)"

clue1 = Length 5 (And (Synonym (Words ["unsuitable"])) (Charade (Synonym (Words ["home"])) (Anagram (Words ["tap"]))))
clue2 = Length 10 (And (Synonym (Words ["palace"])) (Charade (Insertion (Synonym (Words ["carol"])) (Synonym (Words ["county"]))) (Synonym (Words ["working"]))))