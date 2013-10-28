module BioInfo.Fasta 
(
     readFasta
   , readFasta'
) where

-------------------------------------------------------------------------------

import Control.Monad
import Text.ParserCombinators.Parsec

-------------------------------------------------------------------------------

readFasta :: FilePath -> IO [(String, String)]
readFasta file = do contents <- readFile file
                    return $ either (error "Parse error") id 
                           $ parse fastaParser "Parse error" contents

readFasta' :: FilePath -> Int -> IO (String, String)
readFasta' file index = do entries <- readFasta file
                           return $ entries !! index

-------------------------------------------------------------------------------

fastaParser :: GenParser Char st [(String, String)]
fastaParser = do result <- many fastaEntry
                 return result

fastaEntry :: GenParser Char st (String, String)
fastaEntry = do char '>'
                label <- many (noneOf "\n")
                newline
                ds <- many (noneOf ">")
                return (label, filter (/= '\n') ds)

