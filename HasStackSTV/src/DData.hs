module DData (
    zipSnd
    , takeModified
)where

zipSnd :: [(a, b)] -> [(d,c)] -> [(a,(b,c))]
zipSnd ((a,x):xs) ((_,y):ys) = (a,(x,y)) : zipSnd xs ys
zipSnd [] _ = []
zipSnd _ [] = []


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
