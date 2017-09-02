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
    contents <- readFile (head args)
    let election = fromBallot contents
    (print . runElection) election

