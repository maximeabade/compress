{- |
  Module      : LZ.LZ78
  Description : An implementation of LZ78 method
  Maintainer  : MAX
-}
module LZ.LZ78(compress, uncompress) where
findPrefix :: String -> [String] -> Maybe Int
findPrefix prefix dictionary = findPrefix' prefix dictionary 0

findPrefix' :: String -> [String] -> Int -> Maybe Int
findPrefix' _ [] _ = Nothing
findPrefix' prefix (x:xs) index
  | prefix `isPrefixOf` x = Just index
  | otherwise            = findPrefix' prefix xs (index + 1)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (a:as) (b:bs) = a == b && isPrefixOf as bs



-- | LZ78 compress method
compress :: String -> [(Int, Char)]
--compress _ = undefined -- TODO
compress = compress' [] []

compress' :: [String] -> String -> String -> [(Int, Char)]
compress' _ _ [] = []  -- Fin de la chaîne d'entrée
compress' dictionary currentPrefix (x:xs) =
  case findPrefix (currentPrefix ++ [x]) dictionary of
    Just index -> compress' dictionary (currentPrefix ++ [x]) xs
    Nothing    -> (maybe 0 id (findPrefix currentPrefix dictionary), x) : compress' (dictionary ++ [currentPrefix ++ [x]]) [] xs




-- | LZ78 uncompress method
-- If input cannot be uncompressed, returns `Nothing`

--from now it is where i write
--uncompress has entry parameters : [(Int, Char)] and returns Maybe String meaning it returns either a string or nothing
--the string that is returned must follow LZ78 rules
uncompress :: [(Int, Char)] -> Maybe String
uncompress [] = Just "" -- Empty input string results in empty output
uncompress ((0, c):xs) = case uncompress xs of
  Just s -> Just (c:s)
  Nothing -> Nothing
uncompress ((i, c):xs) = case uncompress xs of
  Just s -> Just (take i s ++ [c] ++ drop i s)
  Nothing -> Nothing

