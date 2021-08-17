sumOfSquares x y = xsquare + ysquare
  where xsquare = x * x
        ysquare = y * y

lowest :: Ord a => [a] -> a -- Ord means a type that has an order defined on it, or a type that is comparable
lowest [] = error "The list is empty"
lowest [x] = x
lowest(x:xs)
   | x <= lowest xs = x
   | otherwise = lowest xs  
-- *Main> lowest []
-- *** Exception: The list is empty
-- *Main> lowest [1]
-- 1
-- *Main> lowest [1, 2, 3]
-- 1
-- *Main> lowest [3, 2, 1]
-- 1
-- *Main> lowest [5, 7, 3, 8, 9]
-- 3

-- MORE EFFECTIVE IMPLEMENTATION WITH WHERE
lowestTwo :: Ord a => [a] -> a -- Ord means a type that has an order defined on it, or a type that is comparable
lowestTwo [] = error "The list is empty"
lowestTwo [x] = x
lowestTwo(x:xs)
   | x <= lowestRest = x  
   | otherwise = lowestRest
   where
     lowestRest = lowestTwo xs  -- bindings are visible across guards

    -- The = bindings work globally
    -- but bindings inside functions should
    -- be done with where and let
    -- since they are valid across guards
    -- = bindings dont wokr inside functions

 --Syntax of let expressions:
 --Let var1 = expression1
    -- var2 = expression2
    -- ...
 -- in expression   

sumOfSquaresTwo x y = let xsquare = x * x
                          ysquare = y * y
                      in xsquare + ysquare
    -- let bindings are only visible are only visible
    -- in the expression after the "in"
    -- but if you don't have guards, it is recommended
    -- to use let instead of guard, since it is more readable
    -- but if you have guards use where

--Syntax of a case expression:
-- case expression of pattern1 -> result1
                   -- pattern2 -> result2
                   -- pattern3 -> result3

    --Example of case expression function
 --Write a function takeonly that takes in an integer and
 -- a list and returns a list containing the first 
 -- n elements of the input list

-- takeonly::Integer -> [a] -> [a]
-- takeonly n xs = case (n, xs) of  --NO SIRVE
-- 	               (0, _) -> [] -- when n=0, it doesnt matter what the list is, we are still going to return an empty list
-- 	               (_, []) -> [] -- this is where you have an n, but the list itself is empty
--                   (n, x:xs') -> x:takeonly(n-1)xs' --rename xs to xs'
 -- > takeonly3 [1..10]
 -- [1, 2, 3]
 -- > takeonly 100 "Spartans"
 -- "Spartans"

 --2nd solution with simpler syntax and no cases and built in take function
 -- takeonly::Integer -> [a] -> [a]
 -- takeonly 0 xs = []
 -- takeonly n [] = []
 -- takeonly n (x:xs) = x : takeonly (n-1) xs                 

-- Write a function index that takes an element and a list as arguments.
-- The function returns the index of the first occurrence of the given element in the list.
-- If the element is not in the list, the function returns -1
index::Eq a => a->[a]->Int
index _ [] = -1
index y (x:xs)
   | y == x = 0
   | otherwise = let indexRest = index y xs
                 in case indexRest of
                   -1 -> -1
                   _ -> 1 + indexRest

-- *Main> index 8[1,2,9,4]
-- -1
-- *Main> index 9[1,2,9,4]
-- 2
-- *Main> index 1[1,2,9,4]
-- 0
-- *Main> index 'S' "Go Spartans!"
-- 3
-- *Main>

-- Write a function index that takes an element and a list as arguments.
-- The function returns the index of the first occurrence of the given element in the list.
-- If the element is not in the list, the function returns Nothing

indexTwo :: Eq a => a -> [a]-> Maybe Int
indexTwo _ [] = Nothing
indexTwo y (x:xs)
   | y == x = Just 0
   | otherwise = let indexTwoRest = indexTwo y xs
                     in case indexTwoRest of
                      Nothing-> Nothing
                      Just i-> Just (1 + i)
-- *Main> indexTwo 8[1, 2, 9, 4]
-- Nothing
-- *Main> indexTwo 9[1,2,9,4]
-- Just 2
-- *Main> indexTwo 1 [1, 2, 9, 4]
-- Just 0
-- *Main> indexTwo 'S' "Go Spartans!"
-- Just 3
-- *Main>

lowestThree :: Ord a => [a] -> Maybe a
lowestThree [] = Nothing
lowestThree [x] = Just x
lowestThree (x:xs) 
   | x < lowestThreeRest = Just x
   | otherwise = Just lowestThreeRest
   where
   Just lowestThreeRest = lowestThree xs

-- *Main> lowestThree [2, 4, 1, 5]
-- Just 1
-- *Main> lowestThree []
-- Nothing
-- *Main> lowestThree [9]
-- Just 9
-- *Main>

-- mystery::Int -> Int -> Int
-- iClicker: how many arguments does mystery take? 2
-- the last Int is the return value
-- Explanation 47:30 - 49:00 Meeting March 1, currying

-- *Main> map (*3) [1, 3..6]
-- [3,9,15]                    OR
-- *Main> map (\x-> 3 * x) [1, 3..6]
-- [3,9,15]
-- *Main> filter (> 5) [6, 1, 2, 3, 9]
-- [6,9]
-- zip [1, 2, 3, 4] ['A'..'Z']
-- [(1, 'A'), (2, 'B'), (3, 'C'), (4, 'D')]
-- *Main> zipWith (+) [1..10][10, 11, 12]
-- [11,13,15]
-- *Main> zipWith (\x y -> x*x + y*y) [1, 2] [4..200]
-- [17,29]
-- *Main> foldl (+) 10 [1, 2, 3]
-- 16
-- *Main> foldr (+) 10 [1, 2, 3]
-- 16
-- *Main> foldl (-) 10 [1, 2, 3]
-- 4
-- *Main> foldr (-) 10 [1, 2, 3]
-- -8
-- *Main> foldl1 (+) [1, 2, 3, 4]
-- 10
-- How it works:
-- 1 + 2 = 3
-- 3 + 3 = 6
-- 6 + 4 = 10
-- *Main> foldl1 (-) [1, 2, 3, 4]
-- -2
-- *Main> foldr1 (-) [1, 2, 3, 4]
-- -2
-- *Main> foldr1 (-) [2, 3, 4, 5]
-- -2
-- *Main> foldl1 (-) [2, 3, 4, 5]
-- -10





