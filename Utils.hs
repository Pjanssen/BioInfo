module BioInfo.Utils
(
   -- * List functions
     subsetIndices
   , isPartialMatch
   , combinations
)
where

import Data.List

-------------------------------------------------------------------------------

-- | Gets the starting indices where the given pattern matches a subset of the given list.
--   For example
--
-- >>> subsetIndices "AG" "AAGTCAG"
-- [1,5]
subsetIndices :: (Eq x) => [x] -> [x] -> [Int]
subsetIndices = subsetIndices' 0

subsetIndices' :: (Eq x) => Int -> [x] -> [x] -> [Int]
subsetIndices' _ _ []                      = []
subsetIndices' i p xs | p `isPrefixOf` xs = i : rest
                      | otherwise         = rest
   where rest = subsetIndices' (i + 1) p (drop 1 xs)

-------------------------------------------------------------------------------

-- | Tests if the given lists are equal, with a given maximum number of mismatches.
--   Examples: 
--
-- >>> isPartialMatch 1 "ACT" "ACA"
-- True
-- >>> isPartialMatch 1 "ACT" "ATA"
-- False
isPartialMatch :: (Eq x) => Int -> [x] -> [x] -> Bool
isPartialMatch (-1) _ _ = False
isPartialMatch _ [] []  = True
isPartialMatch _ [] _   = False
isPartialMatch _  _ []  = False
isPartialMatch m (x:xs) (y:ys) | x == y    = isPartialMatch m xs ys
                               | otherwise = isPartialMatch (m - 1) xs ys

-------------------------------------------------------------------------------

-- | Creates a list of all combinations with a given length of the given elements.
--   For example:
--
-- >>> combinations 2 "ACGT"
-- ["AA","AC","AG","AT","CA","CC","CG","CT","GA","GC","GG","GT","TA","TC","TG","TT"]
combinations :: Int -> [x] -> [[x]]
combinations len xs = head $ drop (len - 1) 
                           $ iterate (concatMap (appendLetter xs)) (map (:[]) xs)

appendLetter :: [x] -> [x] -> [[x]]
appendLetter xs ys = map (\y -> ys ++ [y]) xs

-------------------------------------------------------------------------------
