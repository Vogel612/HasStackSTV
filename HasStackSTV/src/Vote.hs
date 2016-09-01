module Vote where

data Preference = Preference {
   first :: Int,
   second :: Int,
   third :: Int
} | None deriving (Eq, Show)

prefFromString :: String -> Preference
prefFromString line = case words line of
    f:s:t:_ ->  Preference (read f) (read s) (read t)
    f:s:_ -> Preference (read f) (read s) 0
    f:_ -> Preference (read f) 0 0
    _ -> None

data Vote = Vote {
    pref :: Preference
    , count :: Int
} deriving (Eq, Show)

fromString :: String -> Vote
fromString line = case words line of
    c:r:_ -> Vote (prefFromString r) (read c)
    _ -> Vote None 0
