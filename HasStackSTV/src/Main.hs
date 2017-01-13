module Main (
    main
) where

import System.IO(readFile, getContents)
import Vote(fromString, combineVotes)
import Election(Election(..), runElection, ElectionResults(..))
import Candidate(Candidate(..))

main :: IO ()
main = do
    contents <- readFile "election-data.blt"
    let ln = lines contents
    let e = head ln
    let votes = combineVotes (map fromString $ takeWhile (\f -> f /= "0" && f /= "0\r") $ tail ln) []
    let candidates = map (\(i, n) -> Candidate n i) $ zip [1..] $ tail $ dropWhile (\f -> f /= "0" && f /= "0\r") ln
    let election = Election (read $ (words e)!!1) candidates votes
    putStrLn $ "Running Election\n"++(show election)
    let results = runElection election
    putStrLn "finished"
    putStrLn $ show results

