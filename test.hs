-- in terminal:

-- ghci

-- :l test 
-- :r #run
-- :t #func
-- :t truncate 
-> truncate :: (RealFrac a, Integral b) => a -> b

--
{- 
-}

import Data.List
import System.IO

-- Interger
-- Double
-- Float
-- Bool: True False
-- Char '
-- Tuple (two values)

always5 :: Int
always5 = 5

-- List
sumOfNums = sum [1..1000]
-- Concat Lists
primeNumbers = [3,5,7,11]
morePrime = primeNumbers ++ [13,17,19,23,29]
morePrime2 = 2 : morePrimes
lenPrime = length morePrime2
-- reverse x
-- null x
-- morePrime2 !! 1
-- head morePrime2 
-- init morePrime2
-- take 3 morePrime2
-- drop 3 morePrime2

-- Last Val In List
-- init morePrime2
-- 7 `elem` morePrime2
-- maximum
-- take 10 (repeat 2)   [2,2,2,2,2,2,2,2,2,2,2]
-- replicate 10 2
-- [x * 2 | x <- [1..10]]
-- sort
-- filter (>5) morePrime2
-- takeWhile (<= 20) [2,4..]
-- foldl/foldr (*) 1 [2,3,4,5]


-- Tuples
-- fst
-- snd
-- zip x y 	(,) (,)

-- MAIN
main = do
	putStrLn "What's your name?"
	name <- getline
	putStrLn ("Hello" ++ name)

-- Combine Numbers Into A List
favNums = 2 : 7 :21 : 88 : []




-- Built in math functions

piVal = pi
ePow9 = exp 9
logOf9 = log 9
squared9 = 9 ** 2
truncateVal = truncate 9.999
roundVal = round 9.999
ceilingVal = ceiling 9.999
floorVal = floor 9.999

-- sin, cos, tan, asin, 