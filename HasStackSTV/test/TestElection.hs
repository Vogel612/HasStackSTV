module TestElection (electionTests) where

import Data.List(take, intersperse)
import Test.HUnit
import Election
import Vote

electionTests :: Test
electionTests = TestLabel "All Tests for Election" $ TestList
    [combineBehaviour
    , combineMultipleVotes
    , quotaTests
    , takeModifiedTest
    ]

combineBehaviour :: Test
combineBehaviour = TestLabel "Combine Votes" $ TestList [
    TestCase $ assertEqual
        "Combine Simple"
        (Vote (checkedPreference 1 2 3) 5)
        (combine (Vote (checkedPreference 1 2 3) 1) $ Vote (checkedPreference 1 2 3) 4),
    TestCase $ assertEqual
        "Combine With None"
        (Vote (checkedPreference 1 2 3) 2)
        (combine (Vote (checkedPreference 0 0 0) 1) $ Vote (checkedPreference 1 2 3) 1)
    ]

combineMultipleVotes :: Test
combineMultipleVotes = TestLabel "Combine Votelist" $ TestList [
    TestCase $ assertEqual
        "Combine identity"
        [Vote (checkedPreference 1 2 3) 5, Vote (checkedPreference 2 1 3) 5, Vote (checkedPreference 3 1 2) 6]
        (combineVotes [Vote (checkedPreference 1 2 3) 5] [Vote (checkedPreference 2 1 3) 5, Vote (checkedPreference 3 1 2) 6]),
    TestCase $ assertEqual
        "Combine equals"
        [Vote (checkedPreference 1 2 3) 25]
        (combineVotes [Vote (checkedPreference 1 2 3) 10, Vote (checkedPreference 1 2 3) 5] [Vote (checkedPreference 1 2 3) 5, Vote (checkedPreference 1 2 3) 5])
    ]

quotaTests :: Test
quotaTests = TestLabel "Quota Tests" $ TestList [
    TestCase $ assertEqual
        "Simple Quota"
        35.0
        (computeQuota 105.0 2 0.0),
    TestCase $ assertEqual
        "Another simple quota"
        44.575
        (computeQuota 214 3 35.7)
    ]

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
