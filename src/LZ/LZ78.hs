module LZ.LZ78(compress, uncompress) where

--MAINTAINER: MAXIME ABADE <maximeabade.fr><github.com/maximeabade><abademaxim@cy-tech.fr>

import Data.List (isPrefixOf, findIndex)

-- | LZ78 compress method
compress :: String -> [(Int, Char)]
compress "" = []


-- | LZ78 uncompress method
uncompress :: [(Int, Char)] -> Maybe String
uncompress [] = Just ""
uncompress _ = Nothing
