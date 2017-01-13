module TestDData (dataTests) where

import DData
import Test.HUnit
import Data.List(intersperse)

dataTests :: Test
dataTests = TestLabel "Tests for custom data processing" $
    TestList [
        takeModifiedTest
        , zipSndTest]

takeModifiedTest :: Test
takeModifiedTest = TestLabel "Take Modified" $ TestList [
    TestCase $ assertEqual
        "Simplistic case"
        [1..6]
        (takeModified $ [1..6] ++ [6..]),
    TestCase $ assertEqual
        "Back and Forth"
        (take 5 $ intersperse 2 $ repeat 1)
        (takeModified [1, 2, 1, 2, 1, 1]),
    TestCase $ assertEqual
        "Empty List"
        []
        (takeModified []:: [Int]),
    TestCase $ assertEqual
        "Trivial Case"
        [1]
        (takeModified $ repeat 1),
    TestCase $ assertEqual
        "simplistic concatenation"
        [1,2,1]
        (takeModified $ [1,2] ++ repeat 1)
    ]

zipSndTest :: Test
zipSndTest = TestLabel "Zip Second of Tuple" $ TestList[
    TestCase $ assertEqual
        "simplisticCase"
        [(1,(1,2)),(2,(1,2)),(3,(1,2))]
        $ zipSnd (zip [1..3] $ repeat 1) (zip [1..] $ repeat 2),
    TestCase $ assertEqual
        "drops second list's fst"
        [(1,(1,2)),(2,(1,2)),(3,(1,2))]
        $ zipSnd (zip [1..] $ repeat 1) (zip [12, 11, 10] $ repeat 2)
    ]
