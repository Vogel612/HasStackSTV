module Main (
    main
) where

import System.Environment(getArgs)
import System.IO(readFile, getContents)
import Vote(fromString, combineVotes)
import Election(fromBallot, runElection, ElectionResults(..))
import Candidate(Candidate(..))

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (args !! 0)
    let election = fromBallot contents
    putStrLn $show $ runElection election

