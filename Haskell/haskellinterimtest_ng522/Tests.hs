module Tests where

import IC.TestSuite

import Compression


countTestCases
  = [ (1,[1,2]) ==> 1,
      (1,[2,1]) ==> 1
    ]

countAllTestCases
  = [ ("i","mississippi") ==> [('i',4)],
      ("k","mississippi") ==> [('k',0)],
      ("ink","mississippi") ==> [('i',4),('n',0),('k',0)]
    ]

buildTableTestCases
  = [ ("mississippi") ==> [('m',1),('i',4),('s',4),('p',2)]
    ]

mergeTestCases
  = [ ((Leaf 3 'a'),(Leaf 1 'b')) ==> Node 4 (Leaf 1 'b') (Leaf 3 'a'),
      ((Node 5 (Leaf 2 'b') (Leaf 3 'c')),(Leaf 7 'a')) ==> Node 12 (Node 5 (Leaf 2 'b') (Leaf 3 'c')) (Leaf 7 'a')
    ]

reduceTestCases
  = [ ([Leaf 2 'm',Leaf 7 'i',Leaf 7 's',Leaf 2 'p',Leaf 2 ' ',Leaf 1 'n',Leaf 1 'g']) ==> Node 22 (Node 9 (Leaf 2 'm') (Leaf 7 'i')) (Node 13 (Node 4 (Leaf 1 'g') (Node 3 (Leaf 1 'n') (Leaf 2 ' '))) (Node 9 (Leaf 2 'p') (Leaf 7 's'))),
      ([Leaf 1 'n',Leaf 1 'g',Leaf 2 'm',Leaf 2 'p',Leaf 2 ' ',Leaf 7 'i',Leaf 7 's']) ==> Node 22 (Node 8 (Node 4 (Leaf 2 'p') (Leaf 2 ' ')) (Node 4 (Node 2 (Leaf 1 'n') (Leaf 1 'g')) (Leaf 2 'm'))) (Node 14 (Leaf 7 'i') (Leaf 7 's')),
      ([Leaf 2 'a', Leaf 3 'b', Leaf 4 'c']) ==> Node 9 (Leaf 4 'c') (Node 5 (Leaf 2 'a') (Leaf 3 'b'))
    ]

buildTreeTestCases
  = [ ("aaabbbccc") ==> Node 9 (Leaf 3 'c') (Node 6 (Leaf 3 'a') (Leaf 3 'b')),
      ("abcabcabcabc") ==> Node 12 (Leaf 4 'c') (Node 8 (Leaf 4 'a') (Leaf 4 'b')),
      ("abcabcabc") ==> Node 9 (Leaf 3 'c') (Node 6 (Leaf 3 'a') (Leaf 3 'b')),
      ("abcabcabca") ==> Node 10 (Leaf 4 'a') (Node 6 (Leaf 3 'b') (Leaf 3 'c'))
    ]

encodeTestCases
  = [ ("m",fig) ==> [0,1,1],
      ("s",fig) ==> [1,1],
      ("ss",fig) ==> [1,1,1,1],
      ("sis",fig) ==> [1,1,1,0,1,1]
    ]

decodeTestCases
  = [ ([1,1],fig) ==> "s",
      ([1,1,1,1],fig) ==> "ss",
      ([1,1,1,0,1,1],fig) ==> "sis"
    ]


allTestCases
  = [ TestCase  "count"           (uncurry count)
                                  countTestCases

    , TestCase  "countAll"        (uncurry countAll)
                                  countAllTestCases

    , TestCase  "buildTable"      (buildTable)
                                  buildTableTestCases

    , TestCase  "merge"           (uncurry merge)
                                  mergeTestCases

    , TestCase  "reduce"          (reduce)
                                  reduceTestCases

    , TestCase  "buildTree"       (buildTree)
                                  buildTreeTestCases

    , TestCase  "encode"          (uncurry encode)
                                  encodeTestCases

    , TestCase  "decode"          (uncurry decode)
                                  decodeTestCases
    ]

runTests = mapM_ goTest allTestCases

main = runTests
