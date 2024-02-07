{- |
  Module      : LZ.LZ78
  Description : An implementation of LZ78 method
  Maintainer  : ???
-}
module LZ.LZ78(compress, uncompress) where

-- | LZ78 compress method
compress :: String -> [(Int, Char)]
compress _ = undefined -- TODO
compress [] = []



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

