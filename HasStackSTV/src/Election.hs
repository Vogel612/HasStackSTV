module Election where

import Vote
import Data.List

data Election = Election {
    candidates :: [Candidate],
    votes :: [Vote],
    seats :: Int
} deriving (Eq, Show)

data Candidate = Candidate {
    name :: String,
    idx :: Int
} | Lost deriving (Eq, Show)

data Round = Round {
    number :: Int,
    candidateScores :: [(Candidate, Float)]
} deriving (Eq, Show)

data ElectionResults = ElectionResults {
    electedCandidates :: [Candidate],
    rounds :: [Round]
} deriving (Eq, Show)

runElection :: Election -> ElectionResults
runElection = undefined

addCandidate :: Election -> Candidate -> Election
addCandidate = undefined
--addCandidate elect cand = Election (distinct [elect.candidates, cand]) elect.votes elect.seats

addVotes :: Election -> Vote -> Election
addVotes elect vote = Election (candidates elect) (combineVotes (votes elect) [vote]) (seats elect)

combineVotes :: [Vote] -> [Vote] -> [Vote]
combineVotes first second = map (foldl combine $ Vote None 0) $
    groupBy (\v1 v2 -> pref v1 == pref v2) (first++second++[])

combine :: Vote -> Vote -> Vote
combine v1 v2 = Vote (selectPref v1 v2) $ count v1 + count v2

selectPref :: Vote -> Vote -> Preference
selectPref v1 v2
    | pref v1 == None = pref v2
    | otherwise = pref v1
