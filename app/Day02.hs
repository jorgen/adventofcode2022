module Day02 (runDay02) where

import Data.FileEmbed
import qualified Data.ByteString.Char8 as C 
import qualified Data.ByteString as BS

data RPS = Rock | Scissors | Paper deriving(Show, Eq)

inputFile :: BS.ByteString
inputFile = $(embedFile "app/input02.txt")

getWinningRPSForRPS :: RPS -> RPS
getWinningRPSForRPS Rock = Paper
getWinningRPSForRPS Paper = Scissors
getWinningRPSForRPS Scissors = Rock

getLosingRPSForRPS :: RPS -> RPS
getLosingRPSForRPS Rock = Scissors
getLosingRPSForRPS Paper = Rock
getLosingRPSForRPS Scissors = Paper

getRPSForInstruction :: RPS -> Char -> Maybe RPS
getRPSForInstruction rps instruction
    | instruction == 'X' = return $ getLosingRPSForRPS rps
    | instruction == 'Y' = return rps
    | instruction == 'Z' = return $ getWinningRPSForRPS rps
getRPSForInstruction _ _ = Nothing

getRPS :: Char -> Maybe RPS
getRPS a | a == 'A' = Just Rock
         | a == 'B' = Just Paper
         | a == 'C' = Just Scissors
getRPS _ = Nothing

getPair_ :: [Char] -> [Char] -> Maybe (Char, Char)
getPair_ [x] [y] = Just (x,y)
getPair_ _ _ = Nothing

getPair :: [BS.ByteString] -> Maybe (Char, Char)
getPair [x, y] = getPair_ (C.unpack x) (C.unpack y)
getPair _ = Nothing

getRPSPair :: BS.ByteString -> Maybe(RPS,RPS)
getRPSPair a = do
    let list = C.split ' ' a
    (pf, ps) <- getPair list
    first <- getRPS pf 
    second <- getRPSForInstruction first ps 
    return (first, second)

getRSPValue :: RPS -> Int
getRSPValue Rock = 1
getRSPValue Paper = 2
getRSPValue Scissors = 3

isWinning :: (RPS, RPS) -> Bool
isWinning (a,b) | a == b = False
isWinning (Scissors, Rock) = True
isWinning (Paper, Scissors) = True
isWinning (Rock, Paper) = True
isWinning _ = False

getWinningScore :: (RPS, RPS) -> Int
getWinningScore (a, b) | a == b = 3
                       | isWinning (a,b) = 6
getWinningScore _ = 0

getScoreForRound :: (RPS, RPS) -> Int
getScoreForRound (a, b) = getRSPValue b + getWinningScore (a, b)

runDay02 :: IO()
runDay02 = do
    let contents = inputFile
    let lists = C.split '\n' contents
    let rpslist = Prelude.map getRPSPair lists
    let score = fmap getScoreForRound <$> rpslist
    let thesum = sum <$> sequence score
    print $ show rpslist
    print $ show score 
    print $ show thesum