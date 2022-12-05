module Day04 (runDay04) where

import Data.FileEmbed
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C 
import Data.List (nub)
import qualified Data.Char as CH
import qualified Data.Word as BS

inputFile :: BS.ByteString
inputFile = $(embedFile "app/input04_test.txt")

makeSplitMaybeTuples :: [BS.ByteString] -> Maybe (BS.ByteString, BS.ByteString)
makeSplitMaybeTuples [x,y] = Just (x,y)
makeSplitMaybeTuples _ = Nothing

makeSplits :: [BS.ByteString] -> [(BS.ByteString, BS.ByteString)]
makeSplits x = catMaybes $ map makeSplitMaybeTuples listSplit
    where 
        listSplit = map C.split ',' x


runDay04 = do
    let contents = inputFile
    let lists = C.split '\n' contents
    let strGroups = makeSplits lists
    print strGroups