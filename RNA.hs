module BioInfo.RNA
(
   -- * Data types
     AminoAcid(AminoAcid, AminoStop)
   , aminoCode
   -- * Functions
   , toRNA
   , fromRNA
   , codons
   , toAminoAcid
   , toAminoAcids
)
where

import BioInfo.DNA

-------------------------------------------------------------------------------

-- | A data type for a single amino acid.
data AminoAcid = AminoAcid Char
               | AminoStop
   deriving (Eq, Show)

-------------------------------------------------------------------------------

-- | Gets the amino acid code from an AminoAcid.
aminoCode :: AminoAcid -> Char
aminoCode (AminoAcid c) = c
aminoCode AminoStop     = error "Stop does not have an amino code"

-------------------------------------------------------------------------------

-- | Transforms a DNA string into an RNA string.
toRNA :: Genome -> Genome
toRNA []       = []
toRNA ('T':ns) = 'U' : toRNA ns
toRNA (n:ns)   = n : toRNA ns

-------------------------------------------------------------------------------

-- | Transforms an RNA string into a DNA string.
fromRNA :: Genome -> Genome
fromRNA []       = []
fromRNA ('U':ns) = 'T' : fromRNA ns
fromRNA (n:ns)   = n : fromRNA ns

-------------------------------------------------------------------------------

-- | Transforms a genome into a list of codons (3-mers).
--
-- >>> codons "AUCGACUGC"
-- ["AUC","GAC","UGC"]
codons :: Genome -> [Genome]
codons []            = []
codons (_:[])        = []
codons (_:_:[])      = []
codons (n1:n2:n3:ns) = [n1, n2, n3] : codons ns

-------------------------------------------------------------------------------

-- | Translates a codon into an `AminoAcid`.
toAminoAcid :: Genome -> AminoAcid
toAminoAcid ('A':'A':'A':[]) = AminoAcid 'K'
toAminoAcid ('A':'A':'C':[]) = AminoAcid 'N'
toAminoAcid ('A':'A':'G':[]) = AminoAcid 'K'
toAminoAcid ('A':'A':'U':[]) = AminoAcid 'N'
toAminoAcid ('A':'C':'A':[]) = AminoAcid 'T'
toAminoAcid ('A':'C':'C':[]) = AminoAcid 'T'
toAminoAcid ('A':'C':'G':[]) = AminoAcid 'T'
toAminoAcid ('A':'C':'U':[]) = AminoAcid 'T'
toAminoAcid ('A':'G':'A':[]) = AminoAcid 'R'
toAminoAcid ('A':'G':'C':[]) = AminoAcid 'S'
toAminoAcid ('A':'G':'G':[]) = AminoAcid 'R'
toAminoAcid ('A':'G':'U':[]) = AminoAcid 'S'
toAminoAcid ('A':'U':'A':[]) = AminoAcid 'I'
toAminoAcid ('A':'U':'C':[]) = AminoAcid 'I'
toAminoAcid ('A':'U':'G':[]) = AminoAcid 'M'
toAminoAcid ('A':'U':'U':[]) = AminoAcid 'I'
toAminoAcid ('C':'A':'A':[]) = AminoAcid 'Q'
toAminoAcid ('C':'A':'C':[]) = AminoAcid 'H'
toAminoAcid ('C':'A':'G':[]) = AminoAcid 'Q'
toAminoAcid ('C':'A':'U':[]) = AminoAcid 'H'
toAminoAcid ('C':'C':'A':[]) = AminoAcid 'P'
toAminoAcid ('C':'C':'C':[]) = AminoAcid 'P'
toAminoAcid ('C':'C':'G':[]) = AminoAcid 'P'
toAminoAcid ('C':'C':'U':[]) = AminoAcid 'P'
toAminoAcid ('C':'G':'A':[]) = AminoAcid 'R'
toAminoAcid ('C':'G':'C':[]) = AminoAcid 'R'
toAminoAcid ('C':'G':'G':[]) = AminoAcid 'R'
toAminoAcid ('C':'G':'U':[]) = AminoAcid 'R'
toAminoAcid ('C':'U':'A':[]) = AminoAcid 'L'
toAminoAcid ('C':'U':'C':[]) = AminoAcid 'L'
toAminoAcid ('C':'U':'G':[]) = AminoAcid 'L'
toAminoAcid ('C':'U':'U':[]) = AminoAcid 'L'
toAminoAcid ('G':'A':'A':[]) = AminoAcid 'E'
toAminoAcid ('G':'A':'C':[]) = AminoAcid 'D'
toAminoAcid ('G':'A':'G':[]) = AminoAcid 'E'
toAminoAcid ('G':'A':'U':[]) = AminoAcid 'D'
toAminoAcid ('G':'C':'A':[]) = AminoAcid 'A'
toAminoAcid ('G':'C':'C':[]) = AminoAcid 'A'
toAminoAcid ('G':'C':'G':[]) = AminoAcid 'A'
toAminoAcid ('G':'C':'U':[]) = AminoAcid 'A'
toAminoAcid ('G':'G':'A':[]) = AminoAcid 'G'
toAminoAcid ('G':'G':'C':[]) = AminoAcid 'G'
toAminoAcid ('G':'G':'G':[]) = AminoAcid 'G'
toAminoAcid ('G':'G':'U':[]) = AminoAcid 'G'
toAminoAcid ('G':'U':'A':[]) = AminoAcid 'V'
toAminoAcid ('G':'U':'C':[]) = AminoAcid 'V'
toAminoAcid ('G':'U':'G':[]) = AminoAcid 'V'
toAminoAcid ('G':'U':'U':[]) = AminoAcid 'V'
toAminoAcid ('U':'A':'A':[]) = AminoStop
toAminoAcid ('U':'A':'C':[]) = AminoAcid 'Y'
toAminoAcid ('U':'A':'G':[]) = AminoStop
toAminoAcid ('U':'A':'U':[]) = AminoAcid 'Y'
toAminoAcid ('U':'C':'A':[]) = AminoAcid 'S'
toAminoAcid ('U':'C':'C':[]) = AminoAcid 'S'
toAminoAcid ('U':'C':'G':[]) = AminoAcid 'S'
toAminoAcid ('U':'C':'U':[]) = AminoAcid 'S'
toAminoAcid ('U':'G':'A':[]) = AminoStop
toAminoAcid ('U':'G':'C':[]) = AminoAcid 'C'
toAminoAcid ('U':'G':'G':[]) = AminoAcid 'W'
toAminoAcid ('U':'G':'U':[]) = AminoAcid 'C'
toAminoAcid ('U':'U':'A':[]) = AminoAcid 'L'
toAminoAcid ('U':'U':'C':[]) = AminoAcid 'F'
toAminoAcid ('U':'U':'G':[]) = AminoAcid 'L'
toAminoAcid ('U':'U':'U':[]) = AminoAcid 'F'
toAminoAcid c = error $ c ++ " is an unknown codon"

-------------------------------------------------------------------------------

-- | Transforms an RNA string into a list of amino acids.
toAminoAcids :: Genome -> [AminoAcid]
toAminoAcids rna = map toAminoAcid $ codons rna

-------------------------------------------------------------------------------

createAminoAcidFns = do f <- readFile "codons.txt"
                        let ls = lines f
                        putStrLn $ unlines $ map createAminoAcidFn ls

createAminoAcidFn (c1:c2:c3:' ':a:_) = "toAminoAcid ('" ++ [c1] ++ "':'" ++ [c2] ++ "':'" ++ [c3] ++ "':[]) = AminoAcid '" ++ [a] ++ "'"
createAminoAcidFn (c1:c2:c3:' ':_)   = "toAminoAcid ('" ++ [c1] ++ "':'" ++ [c2] ++ "':'" ++ [c3] ++ "':[]) = Stop"
createAminoAcidFn f                  = error $ "Could not create function for " ++ f

-------------------------------------------------------------------------------

