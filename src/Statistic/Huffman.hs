{- |
  Module : Statistic.Huffman
  Description : A module containing specifics for the Huffman compression method
  Maintainer : ???
-}
module Statistic.Huffman(tree) where

import Statistic.EncodingTree


import Data.List (sortOn)
import Statistic.Source


{-
-- | New type : PriorityQueuNode (Frequency) (Leaf)
-- | Help building the tree
-}
data PriorityQueueNode a = PriorityQueueNode Int (EncodingTree a)
  deriving (Eq, Show)


-- | Huffman tree generatio n
-- Parameter : List of something
-- Return HUffman Tree 
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree [] = Nothing
tree symbols = buildHuffmanTree priorityQueueList
  where
    -- Transformation de txt en list : [(a, Int)] 
    lst = orderedCounts symbols
    priorityQueueList = buildPriorityQueue lst


{- 
-- | Merge 2 PriotityQueu Node into one, sum frequency and 
-- Parameter 1 & 2: PriorityQueuNode with lowest frequency
-- Return : Merged PriorityQueNode 
-}
mergeNodes :: PriorityQueueNode a -> PriorityQueueNode a -> PriorityQueueNode a
mergeNodes (PriorityQueueNode freq1 tree1) (PriorityQueueNode freq2 tree2) =
  PriorityQueueNode (freq1 + freq2) (EncodingNode (freq1 + freq2) tree1 tree2)

{- 
-- | Transform the original [Symbol, Occurence] list into a PriotityQueuNode's list  
-- Parameter : [Symbol, Occurence] list
-- Return : list of PriorityQueNode 
-}

-- sortOn agit sur la liste [(a,Int)]
buildPriorityQueue :: Ord a => [(a, Int)] -> [PriorityQueueNode a]
buildPriorityQueue = map (\(x, f) -> PriorityQueueNode f (EncodingLeaf f x)) . sortOn snd

{- 
-- | Generate the Huffman Tree by add each lowest PriorityQueuNode frequency 
-- Parameter : PriorityQueuNode list
-- Return : Huffman Tree
-}
buildHuffmanTree :: Ord a => [PriorityQueueNode a] -> Maybe (EncodingTree a)
buildHuffmanTree [node] = Just $ case node of  -- handle last element, tree is done  
  PriorityQueueNode _ tree -> tree
buildHuffmanTree (node1 : node2 : rest) =       -- with 2 element or more 
  buildHuffmanTree $ sortOn (\(PriorityQueueNode freq _) -> freq) (mergeNodes node1 node2 : rest) -- sort on the frequence with the new elemnt merged 
buildHuffmanTree _ = Nothing   -- Empty tree return Maybe Nothing


