module LZ.LZ78 where

import System.IO
import Data.Map as M
import Prelude as P

compress :: String -> [(Int, Char)]
compress = compress' . textWithSpace
  where
    textWithSpace = (++ " ")
    singleton x y = M.singleton x y
    member x y = M.member x y
    insert x y z = M.insert x y z
    size = M.size


compress' :: String -> [(Int, Char)]
compress' = reverse . third . P.foldl step (singleton "" 0, "", [])
  where 
    -- Ajouter un espace à la fin de la chaîne
    textWithSpace = (++ " ")
    step (dict, pat, log) char =
          let pat' = char:pat
          in if member pat' dict
            then (dict, pat', log)
            else (insert pat' (size dict) dict, "", (dict ! pat, char):log)
    third (_, _, log) = log



uncompress :: [(Int, Char)] -> String
uncompress [] = ""
uncompress codes = helper [] codes
  where
    helper _ [] = ""
    helper dict ((index, char):rest) =
      let entry = case index of
                    0 -> [char]
                    _ -> let (prefix, _) = dict !! (index - 1)
                         in prefix ++ [char]
          newDict = dict ++ [(entry ++ [char], length dict + 1)]
      in entry ++ helper newDict rest