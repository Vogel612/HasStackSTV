module Main (
    main
) where

import Test.HUnit
import TestVote

main :: IO Counts
main = runTestTT $ TestList [basicVote, multiVote, incompleteVote, basicVoteFail]

