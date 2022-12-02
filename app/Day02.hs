module Day02 (runDay02) where
import System.IO
import Data.List.Split

runDay02 :: IO()
runDay02 = do
    handle <- openFile "C:/Projects/advent_of_code/app/input02.txt" ReadMode
    contents <- hGetContents handle
    print contents
    let lists = splitOn "\n" contents
    print $ show lists
    hClose handle