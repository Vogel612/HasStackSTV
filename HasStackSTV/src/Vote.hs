module Vote (
    Preference (..)
    , checkedPreference
    , fromString
    , Vote (..)
    , combineVotes
    , combine
) where

import Data.List(groupBy)

data Preference = Preference {
   first :: Int,
   second :: Int,
   third :: Int
} deriving (Eq, Show)

prefFromString :: String -> Preference
prefFromString line = case words line of
    f:s:t:_ ->  checkedPreference (read f) (read s) (read t)
    f:s:_ -> checkedPreference (read f) (read s) 0
    f:_ -> checkedPreference (read f) 0 0
    _ -> checkedPreference 0 0 0

checkedPreference :: Int -> Int -> Int -> Preference
checkedPreference f s t
    | f <= 0 && s <= 0 && t <= 0 = Preference 0 0 0
    | f <= 0 && s <= 0 && t > 0 = Preference t 0 0
    | f <= 0 && s > 0 && t <= 0 = Preference s 0 0
    | f <= 0 && s > 0 && t > 0 = Preference s t 0
    | f > 0 && s <= 0 && t <= 0 = Preference f 0 0
    | f > 0 && s <= 0 && t > 0 = Preference f t 0
    | f > 0 && s > 0 && t <= 0 = Preference f s 0
    | otherwise = Preference f s t

data Vote = Vote {
    pref :: Preference
    , count :: Double
} deriving (Eq, Show)

fromString :: String -> Vote
fromString line = case words line of
    c:f:s:t:_ -> Vote (checkedPreference (read f) (read s) (read t)) (read c)
    c:f:s:_ -> Vote (checkedPreference (read f) (read s) 0) (read c)
    c:f:_ -> Vote (checkedPreference (read f) 0 0) (read c)
    _ -> Vote (checkedPreference 0 0 0) 0

combineVotes :: [Vote] -> [Vote] -> [Vote]
combineVotes first second = map (foldl combine $ Vote (checkedPreference 0 0 0) 0) $
    groupBy (\v1 v2 -> pref v1 == pref v2) (first++second)

combine :: Vote -> Vote -> Vote
combine v1 v2 = Vote (selectPref v1 v2) $ count v1 + count v2

selectPref :: Vote -> Vote -> Preference
selectPref v1 v2
    | pref v1 == checkedPreference 0 0 0 = pref v2
    | otherwise = pref v1

