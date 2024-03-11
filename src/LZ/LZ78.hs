module LZ.LZ78 where

import Data.List (isPrefixOf, findIndex, elemIndex, elem, nub)

findMinPrefix :: String -> [String] -> Maybe Int
findMinPrefix prefix dictionary =
  let index = findIndex (isPrefixOf prefix) dictionary
  in case index of
    Just i -> if i > 0 then Just (i - 1) else Nothing
    Nothing -> Nothing
arrayToString :: [Char] -> String
arrayToString [] = ""
arrayToString (x:xs) = x : arrayToString xs

miseEnforme :: String -> [(Int, Char)]
miseEnforme [] = []
miseEnforme (x:xs) = (0, x) : miseEnforme xs


estDedans :: (Int, Char) -> [(Int, Char)] -> Bool
estDedans (i, c) xs = any ((== i) . fst) $ filter ((== c) . snd) xs

removalDoubles :: [(Int, Char)] -> [(Int, Char)]
removalDoubles xs = removalDoubles' xs []

removalDoubles' :: [(Int, Char)] -> [(Int, Char)] -> [(Int, Char)]
removalDoubles' [] acc = reverse acc
removalDoubles' (x:xs) acc =
  if estDedans x acc
    then removalDoubles' xs acc
    else removalDoubles' xs (x : acc)

--pour chaque (0,char), si estDoublon, alors son index est rajouté au 0 de tous les caracteres qui le suivent ailleurs dans la string

--compress :: String -> [(Int, Char)]

reference :: [(Int, Char)] -> [(Int, Char)] -> [(Int, Char)]
reference [] _ = []
reference (x:xs) (y:ys) = if x == y then (0, snd x) : reference xs ys else (fst y + 1, snd y) : reference (x:xs) ys


createDictionary :: String -> [(Int, Char)]
createDictionary [] = []
createDictionary myString = removalDoubles $ miseEnforme myString         

uncompress :: [(Int, Char)] -> Maybe String
uncompress [] = Just ""
uncompress ((0, c):xs) = case uncompress xs of
  Just s -> Just (c:s)
  Nothing -> Nothing
uncompress ((i, c):xs) = case uncompress xs of
  Just s -> Just (take i s ++ [c] ++ drop i s)
  Nothing -> Nothing