module Main (
    main
) where

import Test.Hspec (hspec, describe)
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.HUnit (Test(TestList))
import TestVote
import TestElection
import TestCandidate

testCases :: Test
testCases =
    TestList [
        voteTests
        , electionTests
        , candidateOrdering
        , basicTricklePreference
    ]

main :: IO()
main = hspec $ describe "all tests" $ fromHUnitTest testCases
