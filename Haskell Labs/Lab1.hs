first = 1
second = 4
third = first + second --binding statement

square x = x * x   --Ask Haskell :t square
squaredub:: Double -> Double 
squaredub x = x * x    --Ask Haskell :t squaredub

factorial::Integer->Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
-- *Main> factorial 2
-- 2
-- *Main> factorial 4
-- 24

swap:: [a] -> [a]
swap [] = []
swap [x] = [x]
swap (first:second:rest) = second:first:rest

monthname :: Int -> String
monthname 1 = "January"
monthname 2 = "February"
monthname 3 = "March"
monthname 4 = "April"
monthname 5 = "May"
monthname 6 = "June"
monthname 7 = "July"
monthname 8 = "August"
monthname 9 = "September"
monthname 10 = "October"
monthname 11 = "November"
monthname _ = "Invalid Month"

letterGrade :: Int -> String
letterGrade x
 | x >= 90 = "A"    --indentation seems neccesary
 | x >= 80 = "B"
 | x >= 70 = "C"
 | x >= 60 = "D"
 | otherwise = "F"


--Things to review on command prompt
{- 
>(\ x -> x * x) 4
16

>(\x y -> x * x + y * y) 3 4
25

> (\ x y z -> x + 2 * y + 3 * z) 4 5 6.0
32.0

> xs = [1, 2.4, 4]
> xs
[1.0, 2.4, 4.0]

> head xs
1.0

> tail xs
[2.4, 4.0]

> last xs
4.0

> init xs    --the list without the last element
[1, 2.4]

> :t xs
xs :: Fractional a => [a]  --means it supports division

> 1:[]
[1]
--          ------------------------------------
> xs = [1, 2, 3]
> 0:xs   --adds 0 to the list
[0, 1, 2, 3]
>'a':xs  --gives error, different type
>1.2:xs --adds 1.2

ys::[Integer]
ys = [1 ,2, 3]
> 1.2:ys
error

> [1, 2, 3] ++ [4.3, 5, 6]
[1.0, 2.0, 3.0, 4.3, 5.0, 6.0]

> [5..10]
[5, 6, 7, 8, 9, 10]
>['a' ..'z']
"abcdefghijklmnopqrstuvwxyz"
>[1, 3..20]
[1, 3, 5, 7, 9, 11, 13, 15, 17, 19]

-- The list [1, 2, 3] can be written as >1:2:3:[]

>xs = [1, 2, 3, 4]
>x:y:z = xs
>x
1
>y
2
>z
[3, 4]
>xs = [1, 2, 3, 4]
> x:_:z = xs -- underscore wildcard
> x
1
> z
[3, 4]
factorial :: integer -> integer
factorial 0 = 1
factorial n = n * factorial (n-1)
swap:: [a] -> [a]
swap [] = []
swap [x] = [x]
swap (first:second:rest) = second:first:rest
>swap [1, 2, 3]
[2, 1, 3]
> swap ["Hello"]
["Hello"]
> swap []
[]
> swap "Hello"
eHllo
:t "Hello"
"Hello" :: [Char]
-- ---------
monthname :: int -> String
monthname 1 = "January"
monthname 2 = "February"
monthname 3 = "March"
monthname 4 = "April"
monthname 5 = "May"
monthname 6 = "June"
monthname 7 = "July"
monthname 8 = "August"
monthname 9 = "September"
monthname 10 = "October"
monthname 11 = "November"
>monthname 4
"April"
> monthname 12
"*** Exception:"
monthname_="Invalid Month"
> monthname 0
"Invalid Month"
> monthname 54
"Invalid month"
-- ---------------
f parameters
| condition 1 = expression1
| condition 2 = expression2
| condition 3 = expression3
| otherwise = default

-- example
letterGrade :: Int -> String
letterGrade x
| x >= 90 = "A"
| x >= 80 = "B"
| x >= 70 = "C"
| x >= 60 = "D"
| otherwise = "F"

-- HOMEWORK
sos x y = x * x + y * y
>:t sos
sos :: Num a => a -> a -> a
> sos 2 3.0
13.0

mystery [] = []
mystery (x:_) = tail x
> :t mystery
mystery :: [[a]] -> [a]
anothermystery _ x = x
> anothermystery (2/0) 7
7
-}