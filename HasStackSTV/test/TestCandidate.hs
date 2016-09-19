module TestCandidate where

import Candidate
import Test.HUnit
import Vote
import Data.List(permutations)
import qualified Data.Map as M


candidateOrdering :: Test
candidateOrdering = TestLabel "Candidate Ordering" $ TestList [
    TestCase $ assertEqual
        "Lost eq Lost"
        (compare 0 0)
        (compare Lost Lost),
    TestCase $ assertEqual
        "Order-wise equal candidates check for candidates"
        (compare 0 0)
        $ compare (Candidate "foo" 1) (Candidate "bar" 1),
    TestCase $ assertBool
        "Lost is smallest candidate"
        (Candidate "foo" 1 > Lost),
    TestCase $ assertBool
        "Candidates order by number"
        (Candidate "asd" 3 > Candidate "a" 1)
    ]

basicTricklePreference :: Test
basicTricklePreference =
    TestLabel "Trickle single vote" $ TestList [
        TestCase $ assertEqual
            "single vote"
            [(Lost, 0.0), (Candidate "foo" 1, 10.0), (Candidate "bar" 2, 0.0), (Candidate "baz" 3, 0.0)]
            (M.toList $ tricklePreference
                (map (\x -> (x, Hopeful)) testCandidates)
                (Vote (checkedPreference 1 2 3) 10)
                (M.fromList $ map (\x -> (x, 0.0)) testCandidates)),
        TestCase $ assertEqual
            "single lost"
            [(Lost, 10.0), (Candidate "foo" 1, 0.0), (Candidate "bar" 2, 0.0), (Candidate "baz" 3, 0.0)]
            (M.toList $ tricklePreference
                (map (\x -> (x, if x == Lost then Hopeful else Excluded)) testCandidates)
                (Vote (checkedPreference 0 0 0) 10)
                (M.fromList $ map (\x -> (x, 0.0)) testCandidates)),
        TestCase $ assertEqual
            "single vote"
            [(Lost, 1.25), (Candidate "foo" 1, 5.0), (Candidate "bar" 2, 2.5), (Candidate "baz" 3, 1.25)]
            (M.toList $ tricklePreference
                (map (\x -> (x, if x == Lost then Hopeful else Elected 0.5)) testCandidates)
                (Vote (checkedPreference 1 2 3) 10)
                (M.fromList $ map (\x -> (x, 0.0)) testCandidates))
        ]
    where
        testCandidates = [Lost,Candidate "foo" 1,Candidate "bar" 2,Candidate "baz" 3]
        prefs = map (\l -> checkedPreference (head l) (head $ tail l) (head $ tail $ tail l)) $ permutations [1,2,3]
