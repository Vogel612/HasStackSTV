data Vote = Vote {
   first :: Int,
   second :: Int,
   third :: Int
} deriving (Eq, Show)

fromString :: String -> Maybe Vote 
fromString line = case words line of
    f:s:t:_ ->  Just $ Vote (read f) (read s) (read t)
    _ -> Nothing
