module LZ.LZW(compress, uncompress) where
import Data.Char (chr)
import Data.List (isPrefixOf)
import LZ.Dictionaries (empty, ascii, zeroAsChar)

-- | Fonction de compression LZW
compress :: String -> [Int]
compress str = compress' str initDict []

-- | Dictionnaire initial
initDict :: [(String, Int)]
initDict = zip (map return ['\0'..'\xFF']) [0..255]

-- | Fonction de compression récursive
compress' :: String -> [(String, Int)] -> [Int] -> [Int]
compress' "" _ encoded = encoded
compress' str dict encoded =
  let
    (prefix, rest) = findLongestPrefix str dict
    (Just newEntry) = lookup prefix dict
    newEncoded = encoded ++ [newEntry]
    newDict = if null rest
              then dict
              else (prefix ++ [head rest], length dict) : dict
  in compress' rest newDict newEncoded

-- | Fonction pour trouver la plus longue chaîne préfixée dans un dictionnaire
findLongestPrefix :: String -> [(String, Int)] -> (String, String)
findLongestPrefix str dict = go str ""
  where
    go [] prefix = (prefix, [])
    go (c:cs) prefix = case lookup (prefix ++ [c]) dict of
      Just _ -> go cs (prefix ++ [c])
      Nothing -> (prefix, c:cs)

-- | LZW uncompress method
-- Si l'entrée ne peut pas être décompressée, retourne `Nothing`


-- Convertir le dictionnaire ascii en [(Int, [Char])]
asciiDict :: [(Int, [Char])]
asciiDict = zip [0..255] (map return ['\0'..'\xFF'])

-- Si l'entrée ne peut pas être décompressée, retourne `Nothing`
uncompress :: [Int] -> Maybe String
uncompress [] = Just ""
uncompress code = go code asciiDict ""  -- Utilisez `asciiDict` comme dictionnaire
  where
    go [] _ output = Just output
    go (codeNr:rest) dict output =
      case lookup codeNr dict of
        Just phrase -> go rest dict (output ++ phrase)
        Nothing ->
          case lookup (head rest) dict of
            Just nextPhrase ->
              let newEntry = (length dict, output ++ [head nextPhrase])
              in go rest (newEntry : dict) (output ++ [head nextPhrase])
            Nothing -> Nothing