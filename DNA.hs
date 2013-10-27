module BioInfo.DNA (
     Nucleotide
   , Genome
   , complement
   , reverseComplement
   , isReverseComplement
   , getKmers
)
where

-------------------------------------------------------------------------------

-- |A type synonym for a single nucleotide.
type Nucleotide = Char

-- |A type synonym for a genome, i.e. a list of nucleotides.
type Genome = [Nucleotide]

-------------------------------------------------------------------------------

complement :: Nucleotide -> Nucleotide
complement 'A' = 'T'
complement 'T' = 'A'
complement 'G' = 'C'
complement 'C' = 'G'
complement n   = error (n : " is an unknown nucleotide")

-- |Creates the reverse complement of the given genome.
reverseComplement :: Genome -> Genome
reverseComplement genome = map complement $ reverse genome

-- |Tests if the given genomes are reverse complements of eachother.
isReverseComplement :: Genome -> Genome -> Bool
isReverseComplement ns ns' = reverseComplement ns == ns'

-------------------------------------------------------------------------------

-- |Creates a list of all kmers with the given length in the given genome.
getKmers :: Int -> Genome -> [Genome]
getKmers k genome = dropLast (k - 1) (getKmers' k genome)

getKmers :: Int -> Genome -> [Genome]
getKmers' _ []     = []
getKmers' k genome = take k genome : getKmers' k (tail genome)

dropLast :: Int -> [x] -> [x]
dropLast _ []   = []
dropLast 0 xs   = xs
dropLast len xs = dropLast (len - 1) (init xs)

-------------------------------------------------------------------------------