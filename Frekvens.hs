import qualified Data.Map as Map
import Data.List (sortBy)

type FrequencyTable = [(Char, Double)]

count :: String -> FrequencyTable
count s = 
    let total = fromIntegral (length s)
        charMap = Map.fromListWith (+) [(c, 1) | c <- s]
    in Map.toList $ Map.map (/ total) charMap

loadFrequencyTable :: FilePath -> IO FrequencyTable
loadFrequencyTable path = do
    content <- readFile path
    return $ count content

initialGuess :: FrequencyTable -> FrequencyTable -> [(Char, Char)]
initialGuess ft1 ft2 = 
    let sorted1 = map fst $ sortBy (\a b -> compare (snd b) (snd a)) ft1
        sorted2 = map fst $ sortBy (\a b -> compare (snd b) (snd a)) ft2
    in zip sorted1 sorted2

chiSquared :: FrequencyTable -> FrequencyTable -> Double
chiSquared model observed =
    let modelMap = Map.fromList model
        observedMap = Map.fromList observed
        allChars = Map.keys (modelMap `Map.union` observedMap)
        chiSquareForChar c = 
            let o = Map.findWithDefault (1/10000) c observedMap
                e = Map.findWithDefault (1/10000) c modelMap
            in (o - e) ^ 2 / e
    in sum $ map chiSquareForChar allChars