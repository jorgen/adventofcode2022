module Day03 (runDay03, runDay03Part2) where

import Data.FileEmbed
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C 
import Data.List (nub)
import qualified Data.Char as CH


inputFile :: BS.ByteString
inputFile = $(embedFile "app/input03.txt")

splitString :: BS.ByteString -> (BS.ByteString, BS.ByteString)
splitString a = BS.splitAt (BS.length a `div` 2) a

intersectStrThree :: (BS.ByteString, BS.ByteString, BS.ByteString) -> BS.ByteString
intersectStrThree (a,b,c) = BS.pack $ nub $ BS.unpack $ BS.sort $ C.filter (`C.elem` c) $ C.filter (`C.elem` a) b

intersectStr :: (BS.ByteString, BS.ByteString) -> BS.ByteString
intersectStr (a,b) = BS.pack $ nub $ BS.unpack $ BS.sort $ C.filter (`C.elem` a) b

pointForChar :: Char -> Maybe Int
pointForChar a | CH.isAsciiUpper a = Just $ CH.ord a - CH.ord 'A' + 27 
               | CH.isAsciiLower a = Just $ CH.ord a - CH.ord 'a' + 1
pointForChar _ = Nothing

pointsForString :: BS.ByteString -> Maybe Int
pointsForString x = sum <$> sequenceOfMaybes
    where
        listOfMaybes = C.foldr (\c processed -> pointForChar c : processed) ([] :: [Maybe Int]) x
        sequenceOfMaybes = sequence listOfMaybes

runDay03 :: IO()
runDay03 = do
    let contents = inputFile
    let lists = C.split '\n' contents
    let splitted = map splitString lists
    let intersection = map intersectStr splitted
    let pointsForStr = map pointsForString intersection
    let totalSum = sum <$> sequence pointsForStr
    print splitted
    print intersection
    print pointsForStr
    print totalSum

makeSplits :: [BS.ByteString] -> [(BS.ByteString, BS.ByteString, BS.ByteString)]
makeSplits (x:y:z:rest) = (x,y,z) : makeSplits rest
makeSplits _ = []

runDay03Part2 :: IO()
runDay03Part2 = do
    let contents = inputFile
    let lists = C.split '\n' contents
    let groups = makeSplits lists
    let intersection = map intersectStrThree groups
    let pointsForStr = map pointsForString intersection
    let totalSum = sum <$> sequence pointsForStr
    print totalSum 