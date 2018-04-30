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
type CharCount = [(Char,Int)]

wordCharCounts :: Word -> CharCount
wordCharCounts = toList . fromListWith (+) . map (\c -> (head c, length c)) . group . map toLower

sentenceCharCounts :: [Word] -> [CharCount]
sentenceCharCounts = map wordCharCounts

dictCharCounts :: [Word] -> [(Word,CharCount)]
dictCharCounts = map (\w -> (w, wordCharCounts w))

dictWordsByCharCounts :: [(Word,CharCount)] -> [(CharCount,[Word])]
dictWordsByCharCounts = toList . fromListWith (++) . map (\(w,cc) -> (cc, [w]))

wordAnagrams :: Word -> [(CharCount,[Word])] -> [Word]
wordAnagrams w ccs = head [ws | (cc,ws) <- ccs, cc == wordCharCounts w]

main = do args <- getArgs
          let sentence = head args
          putStrLn sentence