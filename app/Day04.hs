module Day04 (runDay04) where

import Data.FileEmbed (embedFile)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C 
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

inputFile :: BS.ByteString
inputFile = $(embedFile "app/input04.txt")

makeSplitMaybeTuples :: [BS.ByteString] -> Maybe (BS.ByteString, BS.ByteString)
makeSplitMaybeTuples [x,y] = Just (x,y)
makeSplitMaybeTuples _ = Nothing

liftMaybe :: (Maybe a, Maybe b) -> Maybe (a, b)
liftMaybe (Just x, Just y) = Just (x,y)
liftMaybe _ = Nothing

makeRange_ :: [BS.ByteString] -> Maybe (Int, Int)
makeRange_ [x, y] = liftMaybe (readMaybe $ C.unpack x :: Maybe Int, readMaybe $ C.unpack y :: Maybe Int)

makeRange :: BS.ByteString -> Maybe (Int, Int)
makeRange x = makeRange_ $ C.split '-' x

makeRanges :: (BS.ByteString, BS.ByteString) -> Maybe((Int, Int), (Int, Int))
makeRanges (x,y) = liftMaybe (makeRange x , makeRange y)

makeSplits :: [BS.ByteString] -> [(BS.ByteString, BS.ByteString)]
makeSplits = mapMaybe (makeSplitMaybeTuples . C.split ',')

isGroupsEncapsulated :: ((Int, Int), (Int, Int)) -> Bool
isGroupsEncapsulated ((a,b), (x,y)) = a <= x && b >= y || x <= a && y >= b

isOverlapped :: ((Int, Int), (Int, Int)) -> Bool
isOverlapped ((a,b), (x,y)) = a <= y && b >= x || x <= b && y >= a

boolBasedInc :: Bool -> Int -> Int
boolBasedInc True a = a + 1
boolBasedInc False a = a

runDay04 = do
    let contents = inputFile
    let lists = C.split '\n' contents
    let strGroups = makeSplits lists
    let groups = mapMaybe makeRanges strGroups
    let theCount = foldr (boolBasedInc.isGroupsEncapsulated) 0 groups
    let theCountPart2 = foldr (boolBasedInc.isOverlapped) 0 groups
    print theCountPart2
