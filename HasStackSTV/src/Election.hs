module Election (
    Election (..)
    , ElectionResults(..)
    , runElection
    , addCandidates
    , addVotes
    , computeQuota
    , fromBallot
    )
where

import DData(zipSnd, takeModified)
import Vote
import Candidate
import Data.Ord(comparing)
import Data.List(sort, nub, groupBy, sortBy, findIndex)
import Data.Maybe(fromMaybe)
import qualified Data.Map as M

data Election = Election {
    seats :: Int,
    candidates :: [Candidate],
    votes :: [Vote]
} deriving (Eq)

instance Show Election where
    show e = "The candidates " ++ (shows (unwords $ map name $ candidates e) $ " are running for " ++ shows (seats e) " positions")

data ElectionResults = ElectionResults {
    electedCandidates :: [Candidate],
    rounds :: [Round]
} deriving (Eq)

instance Show ElectionResults where
    show r = unlines (map show $ rounds r) ++ "\nElected Candidates are: " ++ unwords (map show $ electedCandidates r)

addCandidates :: Election -> [Candidate] -> Election
addCandidates e c = Election s c' v
    where
    s = seats e
    v = votes e
    c' = sort $ nub (candidates e)++c

addVotes :: Election -> [Vote] -> Election
addVotes e v = Election s c votes'
    where
    s = seats e
    c = candidates e
    votes' = v ++ votes e

totalVotes :: [Vote] -> Double
totalVotes x = sum $ map count x

runElection :: Election -> ElectionResults
runElection election = ElectionResults electedCandidates rounds
    where
        rounds = takeModified $ iterate
            (nextRound election)
            (Round $ zip (sort . nub $ candidates election ++ [Lost]) (repeat Hopeful))
        electedCandidates = map fst $ filter (\(c, s) -> s /= Excluded && s /= Hopeful) $ candidateData (last rounds)

{-
    Run a Round of the given election. The result is the "starting configuration" for the next round.
    Assuming the election is finished, this is equivalent to id
-}
nextRound :: Election -> Round -> Round
nextRound election round = if filled == seats election then round else (Round $ map (calcWeight q) d)
    where
    calcWeight :: Double -> (Candidate, (CandidateState, Double)) -> (Candidate, CandidateState)
    calcWeight q' data' = (c,s)
        where
        c = fst data'
        d' = snd data'
        s = if c == Lost then Hopeful else asState $ (getRatio $ fst d') * q' / (snd d')
    filled = countElected round
    s = scores (votes election) round
    q = quota election s
    -- this compression assumes that we have the exact same candidates in both candidateData and scores
    d = zipSnd (sortBy (comparing fst) $ candidateData round) (sortBy (comparing fst) $ M.toList s)
    newRound = (Round $ map (calcWeight q) d)
    result = if newRound /= round then newRound else Round $ cullLowestScoring (candidateData round) s
    -- calcWeight needs to be repeated, until wj * q / vj ~= 1.0 for all elected

cullLowestScoring :: CandidateData -> Scores -> CandidateData
cullLowestScoring cData s = map (\(x,y) -> (x, if x == lowest then Excluded else y)) cData
    where
    asc = M.toAscList s
    idx = fromMaybe 0 $ findIndex (\(x,y) -> x /= Lost) asc
    lowest = fst $ asc !! idx



{-
    The convergent iterative scheme is as follows:
    set wj to 0 for excluded candidates, 1 for hopeful candidates, and their last calculated values of wj for elected candidates.
    (Immediately after election of any candidate the last calculated value is 1 initially.)
    Applying rule 2.3, using these weights,let vj be the total value of votes received by candidate j and let e be the total
    excess.
    Using this value for e, calculate the new quota q using rule 2.5.
    Finally update the weights for elected candidates to values wj = wj * q / vj.
    Repeat the process of successively updating wj, q vj and e until every fraction
    wJ * q / vj , for elected candidates, lies within the limits 0.99999 and 1.00001 (inclusive).

    If no candidate was elected in that manner, exclude the candidate with lowest votes. Resolve ties by PRNG
-}

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

fromBallot :: String -> Election
fromBallot contents = Election seats candidates votes
    where
    ln = lines contents
    e = head ln
    candCount = read $ (words e)!!0
    seats = read $ (words e)!!1
    -- skip the row containing candidates and seats counters
    -- if the first line is not a vote we need to skip it, too, but that's TBD
    votes = combineVotes (map fromString $ takeWhile (\f -> f /= "0" && f /= "0\r") $ tail ln) []
    -- skip the row containing the 0, take as many as advertised, else we get the election title, too
    rawCandidates = take candCount $ tail $ dropWhile (\f -> f /= "0" && f /= "0\r") ln
    candidates = map (\(i, n) -> Candidate n i) $ zip [1..] $ rawCandidates
