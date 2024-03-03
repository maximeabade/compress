{- |
  Module      : LZ.LZ78
  Description : An implementation of LZ78 method
  Maintainer  : MAX
-}

module LZ.LZ78 (compress, uncompress) where

import Data.List (isPrefixOf)

-- | Finds the index of the given prefix in the dictionary.
findPrefix :: String -> [String] -> Maybe Int
findPrefix prefix dictionary = findPrefix' prefix dictionary 0
  where
    findPrefix' _ [] _ = Nothing
    findPrefix' p (x:xs) index
      | p `isPrefixOf` x = Just index
      | otherwise        = findPrefix' p xs (index + 1)

-- | LZ78 compress method
compress :: String -> [(Int, Char)]
compress "" = []
compress (x:xs) = (0, x) : compress' [] [x] xs

compress' :: [String] -> String -> String -> [(Int, Char)]
compress' _ _ [] = []  -- End of input string
compress' dictionary currentPrefix (x:xs) =
  case findPrefix (currentPrefix ++ [x]) dictionary of
    Just index -> compress' dictionary (currentPrefix ++ [x]) xs
    Nothing
      | null currentPrefix -> (0, x) : compress' (dictionary ++ [[x]]) [] xs
      | otherwise -> (maybe 0 id (findPrefix currentPrefix dictionary), x) : compress' (dictionary ++ [currentPrefix ++ [x]]) [x] xs

-- | LZ78 uncompress method (no changes needed here)
uncompress :: [(Int, Char)] -> Maybe String
uncompress [] = Just ""
uncompress ((0, c):xs) = case uncompress xs of
  Just s -> Just (c:s)
  Nothing -> Nothing
uncompress ((i, c):xs) = case uncompress xs of
  Just s -> Just (take i s ++ [c] ++ drop i s)
  Nothing -> Nothing
