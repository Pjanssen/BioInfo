module BioInfo.Utils
(
   subsetIndices
)
where

-- |Gets the starting indices where the given pattern matches a subset of the given list.
subsetIndices :: (Eq x) => [x] -> [x] -> Int -> [Int]
subsetIndices [] _ _                     = []
subsetIndices xs p i | p `isPrefixOf` xs = i : rest
                     | otherwise         = rest
   where rest = subsetIndices (drop 1 xs) p (i + 1)