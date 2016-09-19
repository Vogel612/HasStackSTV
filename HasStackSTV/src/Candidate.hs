module Candidate where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Vote

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
 candidateData :: [(Candidate, CandidateState)]
} deriving (Eq, Show)

{-
    Calculates the candidate scores from their weights and all votes
-}
calculateVotes :: [(Candidate,CandidateState)] ->  [Vote] -> M.Map Candidate Double
calculateVotes candidates votes = do
    let scores = M.fromList $ map (\(x,_) -> (x, 0.0)) candidates
    trickleAllPreferences candidates votes scores

{-
    With the given candidate states applies all the given votes by passing each vote to tricklePreference.
-}
trickleAllPreferences :: [(Candidate, CandidateState)] -> [Vote] -> M.Map Candidate Double -> M.Map Candidate Double
trickleAllPreferences candidates [] score = score
trickleAllPreferences candidates x score =
    trickleAllPreferences candidates (tail x) (tricklePreference candidates (head x) score)

{-
    Returns how many votes were "Lost" because they trickled down through all candidates
-}
totalExcess :: M.Map Candidate Double -> Double
totalExcess scores = fromMaybe 0.0 $ M.lookup Lost scores


{-
    With the given CandidateStates applies a Vote to the given map of scores.
    The CandidateStates must as first entry include (Lost, Hopeful). They must be ordered by Candidate index.

    The vote will be applied in order of it's given preference. Elected candidates will only use up their keepRatio
    of the vote and pass on the rest down the given preference. Votes that are not fully consumed by a hopeful candidate
    will be counted as Lost. Excluded candidates do not consume votes.
-}
tricklePreference :: [(Candidate,CandidateState)] -> Vote -> M.Map Candidate Double -> M.Map Candidate Double
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
    Elected _ -> ratio state
    Hopeful -> 1.0
    Excluded -> 0.0
