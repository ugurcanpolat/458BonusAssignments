{- @Author
   Student Name: Ugurcan Polat
   Student ID : 150140117
   Date: 04.05.2018
-}

import Prelude hiding (Word)
import Data.Char 
import Data.Map hiding (map,filter,foldr)
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

charCountsSubsets :: CharCount -> [CharCount]
charCountsSubsets = map wordCharCounts . wordSubset . createWord
  where
    wordSubset :: Word -> [Word]
    wordSubset [] = [[]]
    wordSubset (c:cs) = nub ([c:cs' | cs' <- wordSubset cs] ++ wordSubset cs)

    createWord :: CharCount -> Word
    createWord cc = foldr (\a b -> a ++ b) "" [replicate n c | (c,n) <- cc]

subtractCounts :: CharCount -> CharCount -> CharCount
subtractCounts a b = [(c,n) | (c,n) <- toList (fromListWith (-) (a ++ b)), n > 0]

parseString :: String -> [Word]
parseString s = readUntilSpace s [""]
  where
    readUntilSpace :: String -> [Word] -> [Word]
    readUntilSpace s acc@(a:ac) = case s of 
      []     -> acc
      (c:cs) -> if c == ' ' 
                  then readUntilSpace cs ([""] ++ acc)
                else readUntilSpace cs ([a ++ [c]] ++ ac)

main = do args <- getArgs
          let str = head args
          let sentence = parseString str