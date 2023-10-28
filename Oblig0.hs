module Oblig0 where

import qualified Data.Set as Set
import Data.List (sortBy)
import Data.Tuple (swap)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Char (isAlpha, toLower)

type Key = [(Char,Char)]
type FrequencyTable = [(Char,Double)]
type Alphabet = String
type Dictionary = Set.Set String

encode :: Key -> String -> String
encode k str = map (maybeConvert k) str
    where maybeConvert key char = maybe char id (lookup char key)

decode :: Key -> String -> String
decode k = encode (map swap k)

count :: String -> FrequencyTable
count s = [(ch, fromIntegral (length (filter (== ch) s)) / fromIntegral (length s)) | ch <- Set.toList (Set.fromList s)]

caesar :: Alphabet -> Integer -> Key
caesar alphabet shift = zip alphabet shifted
    where shifted = drop (fromIntegral shift `mod` length alphabet) alphabet ++ take (fromIntegral shift `mod` length alphabet) alphabet

loadFrequencyTable :: FilePath -> IO FrequencyTable
loadFrequencyTable file = do
    content <- readFile file
    let totalChars = fromIntegral $ length content
        charCounts = Map.fromListWith (+) [(ch, 1) | ch <- content]
        freqTable = Map.toList $ Map.map (\count -> fromIntegral count/ totalChars) charCounts
    return freqTable

initialGuess :: FrequencyTable -> FrequencyTable-> Key
initialGuess model observed = zip (map fst sortedObs) (map fst sortedModel)
    where 
        sortedObs = sortBy (\(_, freq1) (_, freq2) -> compare freq2 freq1) observed
        sortedModel = sortBy (\(_, freq1) (_, freq2) -> compare freq2 freq1) model

chiSquared :: FrequencyTable -> FrequencyTable -> Double
chiSquared model obs = sum [calculateChiSquare ch obsVal | (ch, obsVal) <- obs]
    where
        calculateChiSquare ch obsVal =
            case lookup ch model of
                Just modelVal -> (obsVal - modelVal) ** 2 / modelVal
                Nothing -> obsVal **2


neighbourKeys :: Key -> [Key]
neighbourKeys key = [swapEntries a b key | a <- key, b <- key, a /= b]

swapEntries :: (Char,Char) -> (Char, Char) -> Key -> Key
swapEntries a b key = map (f a b) key
    where
        f par1 par2 par
            | par == par1 = par2
            | par == par2 = par1
            | otherwise = par

greedy :: FrequencyTable -> String -> Key -> Key
greedy model cipherText initKey = 
    if ciBestNeighbour < ciKey initKey 
        then greedy model cipherText bestNeighbour 
        else initKey
    where
        ciKey key = chiSquared model (count (decode key cipherText))
        (bestNeighbour, ciBestNeighbour) = minimum [(key, ciKey key) | key <- neighbourKeys initKey]

loadDictionary :: FilePath -> IO Dictionary
loadDictionary path = do
    fileContent <- readFile path
    let extractWords str = map (map toLower) (filter isValidWord (words str))
        isValidWord word = all isAlpha word
        makeUnique wordsList = Set.fromList wordsList
        wordsList = extractWords fileContent
        uniqueWords = makeUnique wordsList
    return uniqueWords

countValidWords :: Dictionary -> String -> Integer
countValidWords dict text =
    let splitWords input = words . map toSpace $ input
        toSpace char | isAlpha char = toLower char
                    | otherwise = ' '
    in toInteger (length (filter (`Set.member` dict) (splitWords text)))

greedyDict :: Dictionary -> String -> Key -> Key
greedyDict dict cipherText initKey =
    if countBestNeighbour > countKey initKey
        then greedyDict dict cipherText bestNeighbour
        else initKey
    where
        countKey key = countValidWords dict (decode key cipherText)
        (bestNeighbour, countBestNeighbour) = maximum [(key, countKey key) | key <- neighbourKeys initKey]