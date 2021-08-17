lowest :: Ord a => [a] -> Maybe a
lowest [] = Nothing
lowest [x] = Just x
lowest (x:xs) 
   | x < lowestRest = Just x
   | otherwise = Just lowestRest
   where
   Just lowestRest = lowest xs

-- *Main> lowest [1, 2, 3]
-- Just 1
-- *Main> 2 * (lowest [1, 2, 3])
-- ERROR since 2 is not compatible with Just 1
-- HOWEVER, if you use the higher order function fmap:
-- > fmap (*2) (lowest [1, 2, 3])	
-- Just 2
-- > fmap (*2)(lowest [])
-- Nothing
-- > fmap (* 2) [1, 2, 3]
-- [2, 4, 6]

-- Function implemented in manatwo.hs
-- Write a function index that takes an element and a list as arguments.
-- The function returns the index of the first occurrence of the given element in the list.
-- If the element is not in the list, the function returns Nothing
index :: Eq a => a -> [a]-> Maybe Int
index _ [] = Nothing
index y (x:xs)
   | y == x = Just 0
   | otherwise = let indexRest = index y xs
                     in case indexRest of
                      Nothing-> Nothing
                      Just i-> Just (1 + i)

-- Revised with fmap:                      
indexTwo :: Eq a => a -> [a]-> Maybe Int
indexTwo _ [] = Nothing
indexTwo y (x:xs)
   | y == x = Just 0
   | otherwise = fmap (+1) (indexTwo y xs)

-- compose g f x = g (f x)
-- example: 
-- compute = compose (+1) (*2) 
-- > compose 5
-- 5  
compose g f x = g (f x)
compute = compose (+1) (*2)  --equivalent: compute = (+1) . (*2)

-- How would you define a second function using 
-- function composition and the head and tail functions?
-- second = ?
-- Answer: second = head . tail
second::[a] -> a
second = head . tail

{-
How would you define the isHotYear function using function
composition and the (>= 103) and the maximum functions?
The maximum function is a built-in function that returns 
the maximum element of a list.
>isHotYear [60, 55, 75, 78, 100, 102, 104, 80, 75, 60, 55]
True
>isHotYear [60, 55, 75, 78, 100, 102, 100, 80, 75, 60, 55]
False

isHotYear::(Ord a, Num a) => [a] -> Bool
isHotYear = ?
Answer: (>= 103) . maximum	
-}
isHotYear::(Ord a, Num a) => [a] -> Bool
isHotYear = (>= 103) . maximum --This takes the max FIRST, then checks if it is equal to or greater than 103
-- (maximum).(>= 103) would have been INCORRECT

-- CHECK HASKEELL TYPES PIC ON DESKTOP, class MARCH 3, 30:55	