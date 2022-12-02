module Day01 (runDay01) where

import System.IO
import Data.List.Split
import Data.List(sort, sortBy)

readInt :: [Char] -> Integer
readInt a = read a ::Integer

convertListToInt :: [[Char]] -> [Integer]
convertListToInt = map readInt

runDay01 :: IO()
runDay01 = do
    handle <- openFile "C:/Projects/advent_of_code/app/input01.txt" ReadMode
    contents <- hGetContents handle
    let lists = splitOn "\n" contents
    let listsoflists = splitWhen null lists
    let ints = map convertListToInt listsoflists
    let sums = map sum ints
    let sort_sums = sortBy (flip compare) sums
    let top_three = take 3 sort_sums
    print $ head sort_sums
    print $ sum top_three
    hClose handle