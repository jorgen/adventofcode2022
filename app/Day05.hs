module Day05 (runDay05) where

import Data.FileEmbed (embedFile)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C 
import qualified Data.List as L
import qualified Data.Char as CH
import Text.Read (readMaybe)
import Debug.Trace (trace)

debug :: c -> String -> c
debug = flip trace

inputFile :: BS.ByteString
inputFile = $(embedFile "app/input05.txt")

getModelDefinition' :: Char -> Int -> [Int] -> (Int, [Int])
getModelDefinition' x y z | CH.isDigit x = (y + 1, z ++ [y])
                          | otherwise = (y + 1, z)

getModelDefinition :: BS.ByteString -> [Int]
getModelDefinition m = list `debug` show list
    where
        (_, list) = C.foldr (\x (index, theList) -> getModelDefinition' x index theList) (0, []::[Int]) m

addToModel :: Char -> [Char] -> [Char]
addToModel x y | CH.isAlpha x = x:y
               | otherwise = y

fillModel' :: [BS.ByteString] -> Int -> [Char]
fillModel' x column = foldr (\line acc -> case C.indexMaybe line column of
                                                Just c -> addToModel c acc
                                                Nothing -> acc) [] x

fillModel :: [BS.ByteString] -> [Int] -> [[Char]]
fillModel x = map (fillModel' x)

getModel :: Maybe ([BS.ByteString], BS.ByteString, [BS.ByteString]) -> [[Char]]
getModel Nothing = []
getModel (Just (x , y, _)) = fillModel x modelDefinition
    where modelDefinition = getModelDefinition y

splitModelInstructions :: [BS.ByteString] -> Maybe ([BS.ByteString], BS.ByteString, [BS.ByteString])
splitModelInstructions d = do
    index <- getIndexPrefixIsDigit d
    case splitAt index d of
        (model, x : xs) -> return (model, x, xs)
        (_, _) -> Nothing
    
getIndexPrefixIsDigit :: [BS.ByteString] -> Maybe Int
getIndexPrefixIsDigit x = L.findIndex (CH.isDigit . C.head) trimmedList
    where
        trimmedList = map (C.dropWhile CH.isSpace) x

liftMaybe :: (Maybe Int,Maybe Int,Maybe Int) -> Maybe (Int, Int, Int)
liftMaybe (Just a, Just b, Just c) = Just (a,b,c)
liftMaybe _ = Nothing

myreadMaybe :: BS.ByteString -> Maybe Int
myreadMaybe = readMaybe . C.unpack

getInstruction :: [BS.ByteString] -> Maybe (Int, Int, Int)
getInstruction [_,a,_,b,_,c] = liftMaybe (myreadMaybe a,myreadMaybe b, myreadMaybe c)
getInstruction _ = Nothing

getInstructions :: Maybe ([BS.ByteString], BS.ByteString, [BS.ByteString]) -> [Maybe (Int, Int, Int)]
getInstructions (Just (_, _, z)) = map (getInstruction . C.split ' ') z
getInstructions Nothing = []

replace :: [a] -> Int -> a -> [a]
replace xs i e = case splitAt i xs of
   (before, _:after) -> before ++ e: after
   _ -> xs

removeHeadAt :: Int -> [BS.ByteString] -> [BS.ByteString]
removeHeadAt index source = replace source index theBSDropped
    where
        theBS = source !! index
        theBSDropped = BS.drop 1 theBS


myMove :: Int -> Int -> [BS.ByteString] -> [BS.ByteString]
myMove from to source = trace ("mymove " ++ show from ++ show to ++ show source) replace removed to theInsertBS
    where
        theChar = C.head (source !! from)
        removed = removeHeadAt from source
        theInsertBS = C.cons theChar (source !! to)

myMoveWithSize :: Int -> Int -> Int -> [BS.ByteString] -> [BS.ByteString]
myMoveWithSize count from to source = replace sourceAfterRemove to theInsertBS
    where
        theRemovedChars = BS.take count (source !! from)
        sourceAfterRemove = replace source from (BS.drop count (source !! from))
        theInsertBS = BS.append theRemovedChars (source !! to)




applyNtimes :: (Num n, Ord n) => n -> (a -> a) -> a -> a
applyNtimes 1 f x = f x
applyNtimes n f x = f (applyNtimes (n-1) f x)

executeInstruction :: [BS.ByteString] -> Maybe (Int, Int, Int) -> [BS.ByteString]
executeInstruction model (Just (move, from, to)) = trace ("move " ++ show move ++ " - " ++ show from ++ " : " ++ show to ++ " : " ++ show model) applyNtimes move (myMove (from - 1) (to - 1)) model
executeInstruction _ _ = []

executeInstruction' :: [BS.ByteString] -> Maybe (Int, Int, Int) -> [BS.ByteString]
executeInstruction' model (Just (move, from, to)) = trace ("move " ++ show move ++ " - " ++ show from ++ " : " ++ show to ++ " : " ++ show model) myMoveWithSize move (from - 1) (to - 1) model
executeInstruction' _ _ = []

runDay05 :: IO ()
runDay05 = do
    let theLines = filter (not . BS.null) $ C.lines inputFile
    let model_instr = splitModelInstructions theLines
    let theModel = map C.pack $ getModel model_instr
    let theInstructions = getInstructions model_instr
    let executedModel = foldl executeInstruction theModel theInstructions
    let heads = foldl (\acc bs -> acc ++ [C.head bs]) "" executedModel
    let executedModel2 = foldl executeInstruction' theModel theInstructions
    let heads2 = foldl (\acc bs -> acc ++ [C.head bs]) "" executedModel2
    print model_instr
    print $ getModel model_instr
    print $ getInstructions model_instr
    print heads
    print heads2

    

