{- |
  Module      : RLE
  Description : An implementation of the run-length encoding method
  Maintainer  : ???
-}

module RLE(compress, uncompress) where

import qualified Data.List as List

-- | RLE compress method
compress :: Eq a => [a] -> [(a, Int)]
compress = map(\a -> (head a, length a)) . List.group

-- | RLE uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(a, Int)] -> Maybe [a] OR Nothing --raph c etait pourtant pas dur...
uncompress = concatMap(\(a, n) -> if (n > 0) then replicate n a else Nothing)
