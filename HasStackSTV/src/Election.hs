module Election (
    Election (..),
    ElectionResults(..),
    runElection,
    addCandidates,
    addVotes,
    combine,
    combineVotes,
    computeQuota,
    takeModified)
where

import Vote
import Candidate
import Data.List(sort, nub, groupBy)
import qualified Data.Map as M

data Election = Election {
    seats :: Int,
    candidates :: [Candidate],
    votes :: [Vote]
} deriving (Eq, Show)

data ElectionResults = ElectionResults {
    electedCandidates :: [Candidate],
    rounds :: [Round]
} deriving (Eq, Show)

addCandidates :: Election -> [Candidate] -> Election
addCandidates elect cand = Election (seats elect) (sort $ nub (candidates elect)++cand) (votes elect)

addVotes :: Election -> [Vote] -> Election
addVotes elect votes = addVotes (addVote elect $ head votes) $ tail votes

addVote :: Election -> Vote -> Election
addVote elect vote = Election (seats elect) (candidates elect) (combineVotes (votes elect) [vote])

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

runElection :: Election -> ElectionResults
runElection election = ElectionResults electedCandidates rounds
    where
        rounds = takeModified $ iterate
            (nextRound election)
            (Round $ zip (sort . nub $ candidates election ++ [Lost]) (repeat Hopeful))
        electedCandidates = map fst $ filter (\(c, s) -> s /= Excluded) $ candidateData (last rounds)

{-
    Take items from a list, as long as the last item taken is not the same as the next item in the list.
    Passing an empty list returns an empty list.
-}
takeModified :: Eq a => [a] -> [a]
takeModified (x:xs) = [x] ++ go x xs
    where
    go :: Eq a => a -> [a] -> [a]
    go elem (x':xs')
       | elem == x' = []
       | otherwise  = [x'] ++ go x' xs'
    go elem [] = []
takeModified [] = []

{-
    Run a Round of the given election. The result is the "starting configuration" for the next round.
    Assuming the election is finished, this is equivalent to id
-}
nextRound :: Election -> Round -> Round
nextRound election round = round -- to be implemented
    where
        v = votes election
        q = quota election $ scores round v

{-
    The convergent iterative scheme is as follows:
    set wj to 0 for excluded candidates, 1 for hopeful candidates, and their last calculated values of wj for elected candidates.
    (Immediately after election of any candidate the last calculated value is 1 initially.)
    Applying rule 2.3, using these weights,let vj be the total value of votes received by candidate j and let e be the total
    excess.
    Using this value for e, calculate the new quota q using rule 2.5.
    Finally update the weights for elected candidates to values wj = wj * q / vj.
    Repeat the process of successively updating wj, q vj and e until every fraction
    q / vj , for elected candidates, lies within the limits 0.99999 and 1.00001 (inclusive).

    If no candidate was elected in that manner, exclude the candidate with lowest votes. Resolve ties by PRNG
-}

{-
    Successively run each round. Since they are dependent on one another, we need the results of the
    previous round anyways. The first round must be the round that comes up when all candidates are marked helpful.

    Takes a calculator for the passing quota depending on the excess votes, the entirity of cast votes and the "previous"
    and generates the subsequent rounds by recursively calling itself until all seats have been filled.
-}
runRounds :: (Double -> Double) -> [Vote] -> Round -> [Round]
runRounds quotaFromExcess votes round = undefined

convergeKeepRatios :: M.Map Candidate CandidateState -> [Vote] -> M.Map Candidate CandidateState
convergeKeepRatios = undefined

{-
    Computes the quota necessary to get counted as Elected from the total number of votes,
    the excess votes that went "lost" and the number of seats available for this election
-}
quota :: Election -> Scores -> Double
quota election scores = (t - e) / s
    where
        t = totalVotes $ votes election
        s = fromIntegral(1 + seats election)
        e = totalExcess scores

computeQuota :: Double -> Int -> Double -> Double
computeQuota total seats excess = (total - excess) / fromIntegral (seats + 1)
