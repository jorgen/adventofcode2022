module Day07 (runDay07) where

import Data.FileEmbed
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C 
import Data.List (nub)
import qualified Data.Char as CH
import qualified Data.Word as BS

inputFile :: BS.ByteString
inputFile = $(embedFile "app/input07_test.txt")

runDay07 = do
    let contents = inputFile
    let lists = C.lines contents
    print lists 