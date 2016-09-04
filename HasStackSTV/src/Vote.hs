module Vote (
    Preference (None, first, second, third),
    checkedPreference,
    fromString,
    Vote (Vote, pref, count)
) where

data Preference = Preference {
   first :: Int,
   second :: Int,
   third :: Int
} | None deriving (Eq, Show)

prefFromString :: String -> Preference
prefFromString line = case words line of
    f:s:t:_ ->  checkedPreference (read f) (read s) (read t)
    f:s:_ -> checkedPreference (read f) (read s) 0
    f:_ -> checkedPreference (read f) 0 0
    _ -> None

checkedPreference :: Int -> Int -> Int -> Preference
checkedPreference f s t
    | f <= 0 && s <= 0 && t <= 0 = None
    | f <= 0 && s <= 0 && t > 0 = Preference t 0 0
    | f <= 0 && s > 0 && t <= 0 = Preference s 0 0
    | f <= 0 && s > 0 && t > 0 = Preference s t 0
    | f > 0 && s <= 0 && t <= 0 = Preference f 0 0
    | f > 0 && s <= 0 && t > 0 = Preference f t 0
    | f > 0 && s > 0 && t <= 0 = Preference f s 0
    | otherwise = Preference f s t

data Vote = Vote {
    pref :: Preference
    , count :: Int
} deriving (Eq, Show)

fromString :: String -> Vote
fromString line = case words line of
    c:r:_ -> Vote (prefFromString r) (read c)
    _ -> Vote None 0
