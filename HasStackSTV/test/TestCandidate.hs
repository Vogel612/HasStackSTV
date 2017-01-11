module TestCandidate where

import Candidate
import Test.HUnit (Test(..), assertEqual, assertBool)
import Vote (checkedPreference, Vote(..))
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
            (M.toList $ calculateScores (zip testCandidates $ repeat Hopeful) [(Vote (checkedPreference 1 2 3) 10)]),
        TestCase $ assertEqual
            "single lost"
            [(Lost, 10.0), (Candidate "foo" 1, 0.0), (Candidate "bar" 2, 0.0), (Candidate "baz" 3, 0.0)]
            (M.toList $ calculateScores (map (\x -> (x, if x == Lost then Hopeful else Excluded)) testCandidates)
                [(Vote (checkedPreference 0 0 0) 10)]),
        TestCase $ assertEqual
            "single vote"
            [(Lost, 1.25), (Candidate "foo" 1, 5.0), (Candidate "bar" 2, 2.5), (Candidate "baz" 3, 1.25)]
            (M.toList $ calculateScores
                (map (\x -> (x, if x == Lost then Hopeful else Elected 0.5)) testCandidates)
                [(Vote (checkedPreference 1 2 3) 10)])
        ]
    where
        testCandidates = [Lost,Candidate "foo" 1,Candidate "bar" 2,Candidate "baz" 3]
        prefs = map (\l -> checkedPreference (head l) (head $ tail l) (head $ tail $ tail l)) $ permutations [1,2,3]

countTheElected :: Test
countTheElected =
    TestLabel "Count Elected" $ TestList [
        TestCase $ assertEqual "two out of three" 2 $ countElected
            $ Round [(Lost, Hopeful), (Candidate "foo" 1 , Elected 0.9), (Candidate "bar" 2, Elected 0.9), (Candidate "baz" 3, Excluded)],
        TestCase $ assertEqual "nobody can across it" 0 $ countElected
            $ Round [(Lost, Hopeful), (Candidate "foo" 1 , Hopeful), (Candidate "bar" 2, Hopeful), (Candidate "baz" 3, Excluded)]
    ]
