module Election (

) where

import Vote

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

runElection :: Election -> [Round]
