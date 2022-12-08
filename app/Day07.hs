module Day07 (runDay07) where

import Data.FileEmbed
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C 

inputFile :: BS.ByteString
inputFile = $(embedFile "app/input07_test.txt")

data Shell a = Command a | Dir a | File (name, size)

isDigit' :: BS.ByteString -> Bool
isDigit' a | Just x <- read a ::Int = True
           | otherwise = False

firstIsDigit :: BS.ByteString -> Bool
firstIsDigit a = BS.words a




getFile :: [BS.ByteString] -> Maybe File (name, size)
getFile [a,b] =

getShell :: BS.ByteString -> Maybe Shell
getshell a | BS.isPrefixOf "$ " a = Just Command $ BS.drop 2 a
           | BS.isPrefixOf "dir " a = Just Dir $ BS.drop 4 a
           | firstIsDigit a = 

getFullShell :: [BS.ByteString] -> [Shell]
getFullShell a =

runDay07 = do
    let contents = inputFile
    let lists = C.lines contents
    print lists 