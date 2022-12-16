module Compression where

import Data.List
import Data.Char
import Data.Maybe

data HTree a = Leaf Int a | Node Int (HTree a) (HTree a)
               deriving (Show)

instance Eq (HTree a) where
  t1 == t2 = freqCount t1 == freqCount t2

instance Ord (HTree a) where
  t1 <= t2' = freqCount t1 <= freqCount t2'

type Code = [Int]

freqCount :: HTree a -> Int
freqCount (Leaf n a)
  = n
freqCount (Node n t1 t2)
  = n

testString :: String
testString
  = "mississippi is missing"

--
-- Example Huffman coding tree from the spec.
--
fig :: HTree Char
fig
  = Node 22 (Node 8 (Node 4 (Leaf 2 'p') (Leaf 2 ' '))
                    (Node 4 (Node 2 (Leaf 1 'n') (Leaf 1 'g'))
                            (Leaf 2 'm')))
            (Node 14 (Leaf 7 'i') (Leaf 7 's'))

----------------------------------------------------------------------------

count :: Eq a => a -> [a] -> Int
count x xs
  = length $ filter (==x) xs

countAll :: Eq a => [a] -> [a] -> [(a, Int)]
countAll i xs
  = zip i $ map (`count` xs) i

buildTable :: Eq a => [a] -> [(a, Int)]
buildTable xs
  = countAll (nub xs) xs

merge :: HTree a -> HTree a -> HTree a
merge n@(Node c _ _)  n'@(Node c' _ _) = Node (c+c') n n'
merge n@(Node c _ _)  l@(Leaf c' _)    = Node (c+c') n l
merge l@(Leaf c _)    n@(Node c' _ _)  = Node (c+c') l n
merge l@(Leaf c _)    l'@(Leaf c' _)   = Node (c+c') l l'

reduce :: [HTree a] -> HTree a
-- Pre: The argument list non-empty and sorted based on the ordering function
--      in the Ord instance above.
reduce [t] = t
reduce (t:t':ts)
  = reduce nts
  where
   nt  = merge t t'
   nts = insert nt ts

buildTree :: Eq a => [a] -> HTree a
-- Pre: The list is non-empty
buildTree
  = reduce . sort . toLeaf . buildTable
  where
    toLeaf = map (\(v, c) -> Leaf c v)

-- helper function returns the encoding for a single element of the list
-- returns a Maybe [Int] as in the case that the element is not in the
-- subtree you are searching it needs to represent that in the output somehow
-- due to the precondition the result of encode' on the whole tree will always
-- produce a value so there is no need to consider the Nothing case
encode :: Eq a => [a] -> HTree a -> Code
-- Pre: The tree can encode each of the items the list
encode vs t
  = concatMap (fromJust . ( encode' t)) vs
  where
    encode' :: Eq a => HTree a -> a -> Maybe [Int]
    encode' (Node _ l r) v
      | isJust ls = Just $ 0:fromJust ls
      | isJust rs = Just $ 1:fromJust rs
      | otherwise = Nothing 
      where
        ls = encode' l v
        rs = encode' r v
    encode' (Leaf _ v') v
      | v == v'   = Just []
      | otherwise = Nothing


decode :: Code -> HTree a -> [a]
-- Pre: The code is valid with respect to the tree
decode [] _ = []
decode c t
  = v : decode cs t
  where
    decode' :: Code -> HTree a -> (a, Code)
    decode' cs (Leaf _ v') = (v', cs)
    decode' (c:cs) (Node _ l r) 
      | c == 0    = decode' cs l
      | otherwise = decode' cs r

    (v, cs) = decode' c t

-- converts an int to a 7 bit binary array of ints
binary :: Int -> [Int]
-- Pre: The input is a non negative integer less than 256
binary n = reverse $ map (`mod` 2) $ take 7 $ iterate (`div` 2) n

compressTree :: HTree Char -> [Int]
compressTree (Leaf _ v)   = 1 : (binary $ ord v)
compressTree (Node _ l r) = 0 : compressTree l ++ compressTree r

-- takes in a list of 1s and 0s and outputs the ascii char
-- represented by the denary representation of the number
binToChar :: [Int] -> Char
-- Pre: Each int in the list is either 1 or 0 and the list
--      has length 7 (will still technically work for other lengths)
binToChar b = chr $ sum $ zipWith (*) (reverse b) (iterate (*2) 1)

rebuildTree :: [Int] -> HTree Char
-- Pre: The bitstring ([Int]) is a valid encoding of a Huffman tree
--      of characters
rebuildTree
  = fst . rebuild'
  where
   rebuild' :: [Int] -> (HTree Char, [Int])
   -- Pre: The list of ints is not empty
   rebuild' (1:bs)
     = (Leaf 0 c, bs')
     where
       c   = binToChar $ take 7 bs
       bs' = drop 7 bs
   rebuild' (0:bs) 
     = (Node 0 l r, bs'')
     where
       (l, bs')  = rebuild' bs
       (r, bs'') = rebuild' bs'

-----------------------------------------------------------------------------
-- THE SECRET TEST STRING (GIVEN)...
--

-- string is "+10,000 marks - well done!"

secretString :: String
secretString
  = decode code (rebuildTree tree)
  where
    code = [1,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,
            1,1,1,1,1,1,1,1,1,1,0,1,0,1,1,1,0,0,1,1,1,0,1,1,0,1,1,
            0,1,0,0,1,0,1,1,1,1,1,0,1,0,1,1,0,0,0,1,1,0,1,0,0,0,1,
            1,0,0,0,1,0,1,1,0,0,1,1,0,1,1,0,1,0,0,1,1,1,0,1,0,0,0,
            1,0,1,0,0,0,1,1,0,1,0]
    tree = [0,0,0,0,1,1,1,1,1,0,0,0,1,1,1,0,0,1,0,1,0,1,1,1,0,0,0,
            1,1,1,1,1,1,0,1,1,1,0,0,1,1,1,0,1,1,1,1,1,1,1,0,1,1,1,
            0,0,1,1,1,0,1,1,0,0,1,1,1,0,0,1,0,0,0,0,1,1,1,0,1,0,1,
            1,0,1,0,1,0,0,0,0,1,0,1,1,1,1,0,0,1,0,1,1,1,1,0,0,1,1,
            0,0,0,1,0,1,0,1,1,0,1,1,0,1,0,1,1,0,0,0,1,0,1,0,0,0,0,
            0,1,0,1,0,1,0,1,1,0,0,1,1,1,0,1,1,0,1,1,1,1,0,0,0,0,1,
            0,1,0,1,1,0,0,0,1,1,0,1,1,0,0,0,0]

