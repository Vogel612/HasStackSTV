module TestVote where

import Test.HUnit
import Vote

--main :: IO Counts
--main = runTestTT $ TestList [basicVote, multiVote, incompleteVote, basicVoteFail]

basicVote =
    TestCase $ assertEqual
    "Should Parse simple Vote"
    (Vote (Preference 1 2 3) 1)
    (fromString "1 1 2 3")

multiVote =
    TestCase $ assertEqual
    "Should Parse multi-vote configuration"
    (Vote (Preference 2 3 5) 2)
    (fromString "2 2 3 5")

basicVoteFail =
    TestCase $ assertEqual
    "Should not parse incorrect vote"
    (Vote None 0)
    (fromString "asd")

incompleteVote =
    TestCase $ assertEqual
    "Should parse incomplete vote"
    (Vote (Preference 3 2 0) 4)
    (fromString "4 3 2")
