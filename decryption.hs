import qualified Data.Set as Set
import System.IO


type Dictionary = Set.Set String

loadDictionary :: FilePath -> IO Dictionary
loadDictionary path = do
    content <- readFile path
    let wordList = words content
    return $ Set.fromList wordList


countValidWords :: Dictionary -> String -> Integer
countValidWords dictionary s = countHelper (words s) 0
    where 
        countHelper [] count = count
        countHelper (w:ws) count
            | w `Set.member` dictionary = countHelper ws (count + 1)
            | otherwise               = countHelper ws count


