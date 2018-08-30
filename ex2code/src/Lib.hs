module Lib
    ( f0
    , f1
    , f2
    , take
    , map
    , iterate
    , filterPos
    , filterPosMany
    , flip3
    , Maybe(..)
    , safeHeadList
    , safeHead
    ) where

import Prelude hiding (map, take, iterate, sqrt, Maybe)

-- TASK 1
-- Parametric polymorphism

-- Below are three type signatures. Can you implement them? We
-- say a function or implementation /inhabits/ it's type
-- (signature). How many other inhabitants do these types
-- have? What if we fixed a = Int, does that change your
-- previous answer?
f0 :: a -> a
f0 a = a

f1 :: a -> b -> a
f1 a b = a

f2 :: a -> b -> b
f2 a b = b

-- Rewrite the function "takeInt" from exercice 1 as "take" so
-- that it accepts a list of any type. If you used the
-- built-in function "take" on the last assignment, write your
-- own implementation this time. Be sure to include a type
-- signature. (Hint: If you already wrote takeInt, you won't
-- have to change much.)
take :: Int -> [a] -> [a]
take _ [] = []
take n (x:xs)
    | n < 1 = []
    | otherwise = x : take (n-1) xs

-- The function head :: [a] -> a which returns the first
-- element of a list, is /partial/, meaning it will crash for
-- some inputs. (Which?) One solution could be to make a
-- /total/ function "safeHeadList :: [a] -> [a]" which either
-- gives the head, or nothing. Can you implement it using take?
safeHeadList :: [a] -> [a]
safeHeadList [] = []
safeHeadList (x:_) = [x]

-- The output of safeHeadList is either empty or a singleton,
-- and thus using a list as output-type is a bit misleading. A
-- better choice is Maybe (sometimes called Optional):
data Maybe a = Some a | None deriving (Eq, Show)

-- Implement 'safeHead', representing failure using None.
safeHead :: [a] -> Maybe a
safeHead [] = None
safeHead (x:_) = Some x

-- TASK 2
-- Higher order functions

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

iterate :: (a -> a) -> a -> [a]
iterate f a = a : iterate f (f a)

-- TASK 3
-- Currying and partial application

-- complete the function filterPos
-- that takes a list and returns 
-- a filtered list containing only positive
-- integers (including zero)
-- use partial application to achieve this
filterPos :: [Int] -> [Int]
filterPos l = filter (>=0) l

-- complete the function filterPosMany
-- that takes a list of lists and returns
-- a list of lists with only positive
-- integers (including zero)
-- hint: use filterPos and map
filterPosMany :: [[Int]] -> [[Int]]
filterPosMany l = map filterPos l

flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 f a b c = f c b a

approxSqrt :: Double -> [Double]
approxSqrt x = iterate (\x0 -> x0 - ((x0*x0 - x)/(2*x0))) 100

-- TASK 4
-- Infinite lists

isInt :: Double -> Double -> Bool
isInt d p = abs (fromIntegral (round d) - d) < p

approx :: Double -> [Double] -> Double
approx d (x:y:ys)
    | abs (x-y) < d = y
    | otherwise = approx d (y:ys)

isPerfSq :: Double -> Bool
isPerfSq d = isInt (approx p (approxSqrt d)) p
    where p = 0.000001

accuracy :: Int -> Bool
accuracy x = take x generated == take x [x^2 | x <- [1..]]
                where
             zpd       = zip [1..] (map isPerfSq [1..])
             f (x,y)   = y == True
             generated = fst . unzip $ filter f zpd
