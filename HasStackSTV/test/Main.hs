module Main (
    main
) where

import Test.HUnit
import TestVote
import TestElection

main :: IO Counts
main = runTestTT $ TestList [basicVote, multiVote, incompleteVote, basicVoteFail,
    combineBehaviour, preferenceConstruction]

