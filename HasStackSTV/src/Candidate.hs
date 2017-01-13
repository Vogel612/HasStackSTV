module Candidate (
    Candidate(..)
    , CandidateState (..)
    , CandidateData
    , Scores
    , Round (..)
    , calculateScores
    , scores
    , totalExcess
    , countElected
    , getRatio
    , asState
    )
where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Vote

type Scores = M.Map Candidate Double
type CandidateData = [(Candidate, CandidateState)]

data Candidate = Candidate {
    name :: String,
    number :: Int
} | Lost deriving (Show, Eq)

instance Ord Candidate where
    compare Lost Lost = compare 0 0
    compare c1 Lost = compare (number c1) 0
    compare Lost c1 = compare 0 (number c1)
    compare c1 c2 = compare (number c1) (number c2)

data CandidateState = Elected {
    ratio :: Double
} | Hopeful
  | Excluded deriving (Show, Eq)

data Round = Round {
 candidateData :: CandidateData
} deriving (Eq, Show)

{-
    Calculates the candidate scores from their weights and all votes
-}
calculateScores :: CandidateData ->  [Vote] -> Scores
calculateScores candidates votes = trickleAllPreferences candidates votes initialScore
    where initialScore = M.fromList $ zip (map fst candidates) $ repeat 0.0

{-
    Convenience accessor to calculateScores dropping the need to unwrap Round
-}
scores :: [Vote] -> Round -> Scores
scores votes round = calculateScores (candidateData round) votes

{-
    Count the elected candidates in a round
-}
countElected :: Round -> Int
countElected round = length $ filter (\(c,s) -> s /= Excluded && s /= Hopeful) $ candidateData round

{-
    With the given candidate states applies all the given votes by passing each vote to tricklePreference.
-}
trickleAllPreferences :: CandidateData -> [Vote] -> Scores -> Scores
trickleAllPreferences candidates [] score = score
trickleAllPreferences candidates x score =
    trickleAllPreferences candidates (tail x) (tricklePreference candidates (head x) score)

{-
    Returns how many votes were "Lost" because they trickled down through all candidates
-}
totalExcess :: Scores -> Double
totalExcess scores = fromMaybe 0.0 $ M.lookup Lost scores


{-
    With the given CandidateStates applies a Vote to the given map of scores.
    The CandidateStates must as first entry include (Lost, Hopeful). They must be ordered by Candidate index.

    The vote will be applied in order of it's given preference. Elected candidates will only use up their keepRatio
    of the vote and pass on the rest down the given preference. Votes that are not fully consumed by a hopeful candidate
    will be counted as Lost. Excluded candidates do not consume votes.
-}
tricklePreference :: CandidateData -> Vote -> Scores -> Scores
tricklePreference candidates vote scores = do
    let oldPref = pref vote
    let candidateState = candidates !! first oldPref
    let ratio = getRatio (snd candidateState)
    let keep = ratio * count vote
    let newScores = M.update (\x -> Just (x+keep)) (fst candidateState) scores
    let newPref = checkedPreference (second oldPref) (third oldPref) 0
    let remainder = count vote - keep
    if remainder > 0 then
        tricklePreference candidates (Vote newPref remainder) newScores
    else
        newScores

getRatio :: CandidateState -> Double
getRatio state = case state of
    Elected a -> a
    Hopeful -> 1.0
    Excluded -> 0.0

asState :: Double -> CandidateState
asState ratio
    | (ratio <= 0.0) = Excluded
    | (ratio >= 1.0) = Hopeful
    | otherwise = Elected ratio
