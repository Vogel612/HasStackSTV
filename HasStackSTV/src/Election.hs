module Election (
    Election (..),
    ElectionResults(..),
    runElection,
    addCandidates,
    addVotes,
    combine,
    combineVotes,
    computeQuota)
where

import Vote
import Candidate
import Data.List

data Election = Election {
    candidates :: [Candidate],
    votes :: [Vote],
    seats :: Int
} deriving (Eq, Show)

addCandidates :: Election -> [Candidate] -> Election
addCandidates elect cand = Election (sort $ nub (candidates elect)++cand) (votes elect) (seats elect)

addVotes :: Election -> [Vote] -> Election
addVotes elect votes = addVotes (addVote elect $ head votes) $ tail votes

addVote :: Election -> Vote -> Election
addVote elect vote = Election (candidates elect) (combineVotes (votes elect) [vote]) (seats elect)

combineVotes :: [Vote] -> [Vote] -> [Vote]
combineVotes first second = map (foldl combine $ Vote (checkedPreference 0 0 0) 0) $
    groupBy (\v1 v2 -> pref v1 == pref v2) (first++second)

combine :: Vote -> Vote -> Vote
combine v1 v2 = Vote (selectPref v1 v2) $ count v1 + count v2

totalVotes :: [Vote] -> Double
totalVotes x = sum $ map count x

selectPref :: Vote -> Vote -> Preference
selectPref v1 v2
    | pref v1 == checkedPreference 0 0 0 = pref v2
    | otherwise = pref v1

data ElectionResults = ElectionResults {
    electedCandidates :: [Candidate],
    rounds :: [Round]
} deriving (Eq, Show)

runElection :: Election -> ElectionResults
runElection election = ElectionResults electedCandidates rounds
    where
        cand = sort $ nub (candidates election)++[Lost] -- check for stepped down candidates?
        initialWeighting = map (\x -> (x,Hopeful)) cand
        rounds = runRounds (computeQuota (totalVotes $ votes election) (seats election))
             (votes election) (Round initialWeighting)
        electedCandidates = map fst $ filter (\(c,s) -> s /= Excluded) $ candidateData (last rounds)

runRounds :: (Double -> Double) -> [Vote] -> Round -> [Round]
runRounds quota votes round = undefined

{-
    Computes the quota necessary to get counted as Elected from the total number of votes,
    the excess votes that went "lost" and the number of seats available for this election
-}
computeQuota :: Double -> Int -> Double -> Double
computeQuota total seats excess = (total - excess) / fromIntegral (seats + 1)
