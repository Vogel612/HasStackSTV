module Election (
    Election (..),
    ElectionResults(..),
    runElection,
    addCandidates,
    addVotes,
    combine, combineVotes)
where

import Vote
import Candidate
import Data.List

data Election = Election {
    candidates :: [Candidate],
    votes :: [Vote],
    seats :: Int
} deriving (Eq, Show)

data ElectionResults = ElectionResults {
    electedCandidates :: [Candidate],
    rounds :: [Round]
} deriving (Eq, Show)

runElection :: Election -> ElectionResults
runElection = undefined

addCandidates :: Election -> [Candidate] -> Election
addCandidates elect cand = addCandidates (addCandidate elect $ head cand) $ tail cand

addCandidate :: Election -> Candidate -> Election
addCandidate elect cand = Election (nub (candidates elect)++[cand]) (votes elect) (seats elect)

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
