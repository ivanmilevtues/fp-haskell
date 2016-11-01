import Data.Char(toLower)


isChar :: Char -> Bool
isChar ch
    |toLower ch >= 'a' && toLower ch <= 'z' = True
isChar _ = False

isVowel :: Char -> Bool
isVowel ch
    | toLower ch == 'a' = True
    | toLower ch == 'e' = True
    | toLower ch == 'u' = True
    | toLower ch == 'i' = True
    | toLower ch == 'o' = True
    | toLower ch == 'y' = True
isVowel _ = False


isConsonant :: Char -> Bool
isConsonant ch
    | not (isChar ch) = False
    | isVowel ch = False
isConsonant _ = True


encode :: String -> String
encode [] = []
encode (x:xs)
    | isConsonant x = x : 'o' : x : (encode xs)
encode (x:xs) = x : (encode xs)

-- Bonus #1
encode' :: String -> String
encode' [] = []
encode' (x:xs)
    | isConsonant x = x : 'o' : toLower x : (encode' xs)
encode' (x:xs) = x : (encode' xs)

-- Bonus #2
dropN :: Int -> String -> String
dropN _ [] = []
dropN 0 anyString = anyString
dropN a  (x:xs) = dropN (a - 1) xs


--Assume we'll be decoding only valid words
decode :: String -> String
decode [] = []
decode (x:'o':y:xs)
    | x == y = x : decode xs
decode (x:xs) = x:decode xs
