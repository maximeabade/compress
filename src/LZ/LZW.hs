{- |
  Module      : LZ.LZW
  Description : An implementation of LZW method
  Maintainer  : ???
-}

module LZ.LZW(compress, uncompress) where
import LZ.Dictionaries

example = "Je suis tout puissant"

-- | LZW compress method
compress :: String -> [Int]
compress _ = undefined -- TODO

-- | LZW uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [Int] -> Maybe String
uncompress _ = undefined -- TODO

has:: String -> [Char] -> Int
has word dict = word `elemIndex` dict
 
    
newSequence:: String -> [(Chat,Int)] -> Bool
newSequence sequence dict


compress':: String -> [String] -> [Int]
compress' text  dict = Nothing


{-
lecture caractère

Si dans Disctionnaire
   -> Prendre numero de table correspondant
   -> Letttre suivante 

Si PAs dedans 
  -> Rajout de la lettre

-}