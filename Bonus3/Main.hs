{- @Author
   Student Name: Ugurcan Polat
   Student ID : 150140117
   Date: 04.05.2018
-}

import Prelude hiding (Word)
import Data.Char 
import Data.Map hiding (map)
import Data.List
import System.Environment

type Word      = [Char]
type Sentence  = [Word]
type CharCount = [(Char,Int)]

wordCharCounts :: Word -> CharCount
wordCharCounts = toList . fromListWith (+) . map (\c -> (head c, length c)) . group . map toLower

main = do args <- getArgs
          let sentence = head args
          putStrLn sentence