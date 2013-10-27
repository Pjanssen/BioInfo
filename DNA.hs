module BioInfo.DNA (
     complement
   , reverseComplement
)
where

-------------------------------------------------------------------------------

complement :: Char -> Char
complement 'A' = 'T'
complement 'T' = 'A'
complement 'G' = 'C'
complement 'C' = 'G'
complement n   = error ("Unknown nucleotide " ++ [n])

reverseComplement :: String -> String
reverseComplement genome = map complement $ reverse genome

-------------------------------------------------------------------------------



-------------------------------------------------------------------------------