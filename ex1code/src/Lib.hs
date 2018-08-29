{-# LANGUAGE ParallelListComp #-}

module Lib
    ( add
    , isVowel
    , Status(..)
    , amountOf
    , fib
    , ending
    , takeInt
    , fizzbuzz
    , printFizz
    , listOfEven
    , zipped
    , cartesian
    ) where

import Control.Monad (mapM_)
import Prelude hiding (take)
import Data.List

-- TASK 1
-- Simple functions

-- finish the function "add" that takes two integers
-- and returns the sum of them
add :: Int -> Int -> Int
add n m = n + m

-- complete the function "isVowel" which
-- takes a character and returns True
-- if it's a vowel (English language), False otherwise
-- hint: a string is a list
-- hint2: use `elem` from Prelude
isVowel :: Char -> Bool
isVowel chr = chr `elem` "aeuioyAEIOUY"

data Status = One | Two | Three | None deriving (Show, Eq)

-- complete the function "amountOf" which takes
-- a name and a list of subjects
-- it then pattern matches on the amount of subjects in the list
-- and returns a tuple of a Status (look above, describes the amount of subjects in the list)
-- and the name
amountOf :: String -> [a] -> (Status, String)
amountOf string list = let n = length list in
    (let status | n == 0 = None
                | n == 1 = One
                | n == 2 = Two
                | otherwise = Three
    in (status, string))

-- TASK 2
-- Recursion

-- finish the function "fib" that calculates the
-- nth fibonacci number 
-- assuming that 0th = 0 and 1st = 1
-- do not optimize it
fib :: Int -> Int
fib n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = (fib (n-1)) + (fib (n-2))

-- TASK 3
-- Working with lists

-- complete the function "ing" that takes a list of
-- strings and returns them with the ending -ing
-- if the string is empty, remove it from the list
ending :: [String] -> [String]
ending list = map (\s -> s ++ "ing") (filter (not . null) list)

-- complete the function "takeInt" that
-- an integer n and a list of integers and
-- returns the first n elements of 
-- the list
takeInt :: Int -> [Int] -> [Int]
takeInt n list = take n list

-- implement "fizzbuzz" as described in exercise 1
fizzbuzz :: [String]
fizzbuzz = map (\a -> let s = concat [if a `mod` fst t == 0 then snd t else "" | t <- [(3, "Fizz"), (5, "Buzz")]] in if null s then show a else s) [1..100]

printFizz :: IO ()
printFizz = mapM_ putStrLn fizzbuzz

-- TASK 4
-- List comprehensions

-- create a list "listOfEven" which contains all 
-- even numbers that are equal or greater than 0
-- use a list comprehension
listOfEven :: [Integer]
listOfEven = [n | n <- [0..], even n]

-- create a list of tuples, "zipped"
-- where each tuple contains the nth entry
-- in the lists [1..26] and ['a'..'z']
-- hint: parallel list comprehension
zipped :: [(Int, Char)]
zipped = zip [1..26] ['a'..'z']

-- create a list that contains the cartesian
-- product of the two vectors [4, 6, 8]
-- and [3, 7, 9]
-- use a list comprehension
cartesian :: [(Int, Int)]
cartesian = [(a, b) | a <- [4,6,8], b <- [3,7,9]]
