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

type Quota = Double

addCandidates :: Election -> [Candidate] -> Election
addCandidates (Election s c v) c' = Election s (nub . sort $ c ++ c') v

addVotes :: Election -> [Vote] -> Election
addVotes (Election s c v) v' = Election s c (v ++ v')

totalVotes :: [Vote] -> Double
totalVotes = sum . map count

runElection :: Election -> ElectionResults
runElection election = ElectionResults electedCandidates rounds
    where
        rounds = takeModified $ iterate
            (nextRound election)
            (Round $ zip (nub . sort $ candidates election ++ [Lost]) (repeat Hopeful))
        electedCandidates = map fst $ filter (\(c, s) -> s /= Excluded && s /= Hopeful) $ candidateData (last rounds)

{-
    Run a Round of the given election. The result is the "starting configuration" for the next round.
    Assuming the election is finished, this is equivalent to id
-}
nextRound :: Election -> Round -> Round
nextRound election round
    | countElected round == seats election = round
    | otherwise = result
        where
        calcQuota :: VoteCount -> Quota
        calcQuota = computeQuota (totalVotes $ votes election) (seats election)
        -- FIXME iterate the results of convergeScore calcQuota until they don't change again
        s :: Scores
        s = calculateScores (candidateData round) (votes election)
        -- this compression assumes that we have the exact same candidates in both candidateData and scores
        -- candidateWeightAndVotes :: [(Candidate, (CandidateState, VoteCount))]
        -- candidateWeightAndVotes = zipSnd (sortBy (comparing fst) $ candidateData round) (sortBy (comparing fst) $ M.toList s)

        newRound = Round $ doConverge (votes election) calcQuota (candidateData round) s
        result = if newRound /= round then newRound else Round $ cullLowestScoring (candidateData round) s

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
doConverge :: [Vote] -> (VoteCount -> Quota) -> [(Candidate, CandidateState)] -> Scores -> [(Candidate, CandidateState)]
doConverge allVotes calcQuota weights scores =
    -- get the excess from scores
    if termination newQuota newScores newWeights then
        newWeights
    else
        doConverge allVotes calcQuota newWeights newScores
    where
        newQuota = calcQuota $ totalExcess scores
        newWeights = map (convergeWeight newQuota)
            $ zipSnd (sortBy (comparing fst) weights)
                     (sortBy (comparing fst) $ M.toList scores)
        newScores = calculateScores newWeights allVotes
        termination :: Quota -> Scores -> [(Candidate, CandidateState)] -> Bool
        termination q v w =
            all (\ratio -> ratio <= 1.00001 && ratio >= 0.99999)
                $ map (\(c, wj) -> (getRatio wj * q) / fromMaybe 1.0 (M.lookup c v)) w

{-
    Performs a single step of the convergent iterative scheme on a single candidate with: Quota, (Candidate, (Weight, Votes))
    returns a (Candidate, Weight) that can be used to calculate the new excess, quota and votes
-}
convergeWeight :: Quota -> (Candidate, (CandidateState, VoteCount)) -> (Candidate, CandidateState)
convergeWeight _ (Lost, _) = (Lost, Hopeful)
convergeWeight _ (c,(Excluded, _)) = (c, Excluded)
convergeWeight quota (candidate, (Hopeful, alottedVotes))
    | quota > alottedVotes = (candidate, Hopeful)
    | otherwise = (candidate, Elected 1.0)
convergeWeight quota (candidate, (Elected w, alottedVotes))
    | 0.99999 <= (w * quota / alottedVotes) && (w * quota / alottedVotes) <= 1.00001 = (candidate, Elected w)
    | otherwise = (candidate, Elected $ w * quota / alottedVotes)


{-
    Computes the quota necessary to get counted as elected frm the total number of votes,
    the seats available and the excess votes, that are no more applicable for any particular candidate.
-}
computeQuota :: VoteCount -> Int -> VoteCount -> VoteCount
computeQuota total seats excess = (total - excess) / fromIntegral (seats + 1)

fromBallot :: String -> Election
fromBallot contents = Election seats candidates votes
    where
    ln = lines contents
    e = head ln
    candCount = read $ head $ words e
    seats = read $ words e!!1
    -- skip the row containing candidates and seats counters
    -- if the first line is not a vote we need to skip it, too, but that's TBD
    votes = combineVotes (map fromString $ takeWhile (\f -> f /= "0" && f /= "0\r") $ tail ln) []
    -- skip the row containing the 0, take as many as advertised, else we get the election title, too
    rawCandidates = take candCount $ tail $ dropWhile (\f -> f /= "0" && f /= "0\r") ln
    candidates = map (\(i, n) -> Candidate n i) $ zip [1..] rawCandidates
