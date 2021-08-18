{- 
CS152 HW5
-}

-- function top that takes a list of elements and a threshold as arguments and returns 
-- a new list that contains the elements of the original list that are greater or equal to the given threshold.
top:: Ord a => [a] -> a -> [a]
top [] _ = []
top xs x = filter (>= x) xs

-- function that takes a list and returns a list that contains every other element starting with the first one
skip:: [a] -> [a]
skip [] = []
skip [x] = [x]
skip(x:_:xs) = x:skip xs

-- function that merges two lists by alternating elements from both lists
mix:: [a] -> [a] -> [a]
mix [] ys = ys
mix xs [] = xs
mix [x] [y] = x:y:[]
mix (x:xs) (y:ys) = x:y:mix xs ys

-- funcion that returns a new list with elements common in both lists with no duplicates
inCommon:: Eq a => [a] -> [a] -> [a]
inCommon [] _ = []
inCommon _ [] = []
inCommon (x:xs) ys
    |x `elem` ys = x:filter(/= x) (inCommon xs ys)
    |otherwise = inCommon xs ys

-- function that returns the shortet list
shortest:: Ord a => [[a]] -> Maybe [a]
shortest [] = Nothing
shortest [x] = Just x
shortest (x:y:xs)
    |length(x) < length(y) = shortest (x:xs)
    |otherwise = shortest (y:xs)

data ExpTree  = Atom Double
               | Plus ExpTree  ExpTree
               | Times ExpTree  ExpTree
         deriving Show 
-- function that takes one argument and evaluates based off the data type ExpTree
evaluate:: ExpTree -> Double
evaluate (Atom x) = x
evaluate (Plus x y) = (evaluate x) + (evaluate y)
evaluate (Times x y) = (evaluate x) * (evaluate y)

-- function that takes a list of numbers and returns true if they are all negative
allNegative :: (Ord a, Num a) => [a] -> Bool
allNegative xs = foldr (\x y -> x < 0 && y) True xs

-- function that returns an infinte list that represents the geo sequence given by two numbers
geoSequence:: Num a => a -> a -> [a]
geoSequence x y = map (x*) [y^z | z <- [0..]]
