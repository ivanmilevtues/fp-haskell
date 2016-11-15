{-# OPTIONS_GHC -Wall #-}

module Lists where

import qualified Data.List ()

import Prelude hiding (
        head, tail, null, length, reverse, repeat, replicate,
        concat, sum, maximum, take, drop, elem, (!!)
    )


head :: [Int] -> Int
head []    = error "empty list"
head (x:_) = x


tail :: [Int] -> [Int]
tail []     = error "empty list"
tail (_:xs) = xs


append :: [Int] -> [Int] -> [Int]
append []     ys = ys
append (x:xs) ys = x : (append xs ys)


elementAt :: Int -> [Int] -> Int
elementAt 0 (x:_) = x
elementAt _ []     = error "index greater than length"
elementAt n (_:xs) = elementAt (n - 1) xs


null :: [Int] -> Bool
null [] = True
null _  = False

length :: [Int] -> Int
length [] = 0
length [_] = 1
length (_:xs) = 1 + length(xs)


take :: Int -> [Int] -> [Int]
take _ [] = []
take 1 (x:_) = [x]
take n _ | n < 1 = []
take n (x:xs) = x : take (n-1) xs

take' :: Int -> [Int] -> [Int]
take' 0 _  = []
take' _ [] = []
take' n (x:xs) | n < 0     = []
               | otherwise = x : take' (n-1) xs


drop :: Int -> [Int] -> [Int]
drop _ [] = []
drop 1 (_:xs) = xs
drop n (_:xs) = drop (n - 1) xs

elem :: Int -> [Int] -> Bool
elem _ [] = False
elem n (x:xs) | n == x = True
              | otherwise = elem n xs

reverseHelper :: [Int] -> [Int] -> [Int]
reverseHelper acc []     = acc
reverseHelper acc (x:xs) = reverseHelper (x:acc) xs

reverse :: [Int] -> [Int]
reverse xs = reverseHelper [] xs

reverseStringHelper::String->String->String
reverseStringHelper acc [] = acc
reverseStringHelper acc (x:xs) = (reverseStringHelper (x:acc) xs)


reverseString :: String -> String
reverseString str = "This is the reversed string: " ++ (reverseStringHelper [] str)

concat :: [[Int]] -> [Int]
concat = undefined


replicate :: Int -> Int -> [Int]
replicate 0 _ = []
replicate 1 n = [n]
replicate k n = n : replicate (k - 1) n

interleave :: [Int] -> [Int] -> [Int]
interleave = undefined


sum :: [Int] -> Int
sum [] = 0
sum [x] = x
sum (x:xs) = x + sum xs


maximum :: [Int] -> Int
maximum [] = 0
maximum (x:xs) | x > maximum xs = x
               | otherwise = maximum xs

nub :: [Int] -> [Int]
nub = undefined


delete :: Int -> [Int] -> [Int]
delete _ [] = []
delete 1 (_:xs) = xs
delete n (x:xs) = x : delete (n - 1) xs

difference :: [Int] -> [Int] -> [Int]
difference = undefined


union :: [Int] -> [Int] -> [Int]
union = undefined


intersect :: [Int] -> [Int] -> [Int]
intersect = undefined
