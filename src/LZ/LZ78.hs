module LZ.LZ78(compress, uncompress) where

import Data.List (findIndex, isPrefixOf)
-- | LZ78 compress method
compress :: String -> [(Int, Char)]
compress = compress' []
    where
        compress' :: [String] -> String -> [(Int, Char)]
        compress' _ [] = []
        compress' dict str@(x:xs) =
            case findIndex (`isPrefixOf` str) dict of
                Just ref -> (ref, head xs) : compress' dict xs
                Nothing  -> (0, x) : compress' (dict ++ [str]) xs


-- | LZ78 uncompress method
uncompress :: [(Int, Char)] -> Maybe String
uncompress = fmap concat . mapM expand
  where
    expand (ref, char) = do
      dict <- uncompress' ref
      return (dict ++ [char])

uncompress' :: Int -> Maybe String
uncompress' 0 = Just ""
uncompress' n = fmap (++ [lastChar]) (uncompress' ref)
  where
    (ref, lastChar) = (n - 1, toEnum (n - 1))