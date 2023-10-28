type Key = [(Char, Char)]
type Alphabet = [Char]

caesar :: Alphabet -> Int -> Key
caesar alphabet shift = zip alphabet (drop shift alphabet ++ take shift alphabet)


encode :: Key -> String -> String
encode key str = map (\char -> case lookup char key of
                                Just value -> value
                                Nothing    -> char) str


decode :: Key -> String -> String
decode key = encode (swap key)
    where 
        swap = map (\(a, b) -> (b, a))


main :: IO ()
main = do
    let myAlphabet = ['a'..'z'] ++ ['A'..'Z'] ++ " .,-?!"
    let myKey = caesar myAlphabet 5

    let plaintext = "Hello, world!"
    let ciphertext = encode myKey plaintext

    putStrLn $ "Original: " ++ plaintext
    putStrLn $ "Encoded : " ++ ciphertext
    putStrLn $ "Decoded : " ++ decode myKey ciphertext