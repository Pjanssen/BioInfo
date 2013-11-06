module BioInfo.DNA (
   -- * Types
     Nucleotide
   , Genome
   -- * Functions
   , reverseComplement
   , isReverseComplement
   , getKmers
   , getSkews
)
where

import BioInfo.List

-------------------------------------------------------------------------------

-- | A type synonym for a single nucleotide.
type Nucleotide = Char

-- | A type synonym for a genome, i.e. a list of nucleotides.
type Genome = [Nucleotide]

-------------------------------------------------------------------------------

-- | Creates the reverse complement of the given genome.
reverseComplement :: Genome -> Genome
reverseComplement genome = map complement $ reverse genome

-- | Tests if the given genomes are reverse complements of eachother.
isReverseComplement :: Genome -> Genome -> Bool
isReverseComplement ns ns' = reverseComplement ns == ns'

complement :: Nucleotide -> Nucleotide
complement 'A' = 'T'
complement 'T' = 'A'
complement 'G' = 'C'
complement 'C' = 'G'
complement n   = error (n : " is an unknown nucleotide")

-------------------------------------------------------------------------------

-- | Creates a list of all kmers with the given length in the given genome.
getKmers :: Int -> [Genome] -> [[Genome]]
getKmers = subsets

-------------------------------------------------------------------------------

-- | Gets the skew value at each point in the given genome.
--   For example:
--
-- >>> getSkews "ACGTAGAC"
-- [0,0,-1,0,0,0,1,1,0]
getSkews :: Genome -> [Int]
getSkews = scanl (\s n -> s + skew n) 0

skew :: Nucleotide -> Int
skew 'G' = 1
skew 'C' = -1
skew _   = 0

-------------------------------------------------------------------------------
