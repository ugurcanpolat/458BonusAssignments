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
wordAnagrams w ccs = case result of 
    [] -> []
    _  -> head result
  where
    comp = sortBy (\(a,_) (b,_) -> compare a b) $ wordCharCounts w
    result = [ws | (cc,ws) <- ccs, sortBy (\(a,_) (b,_) -> compare a b) cc == comp]

charCountsSubsets :: CharCount -> [CharCount]
charCountsSubsets = map wordCharCounts . wordSubset . createWord
  where
    wordSubset :: Word -> [Word]
    wordSubset [] = [[]]
    wordSubset (c:cs) = nub ([c:cs' | cs' <- wordSubset cs] ++ wordSubset cs)

createWord :: CharCount -> Word
createWord cc = foldr (\a b -> a ++ b) "" [replicate n c | (c,n) <- cc]

subtractCounts :: CharCount -> CharCount -> CharCount
subtractCounts a b = [(c,abs n) | (c,n) <- toList (fromListWith (-) (a ++ b)), n /= 0]

sentenceAnagrams :: [Word] -> [(CharCount,[Word])] -> [[Word]]
sentenceAnagrams sent dict = helper sentsubs sentcc [] [[]]
  where
    sentcc   = toList $ fromListWith (+) $ foldr (\a b -> a ++ b) [] [a | a <- sentenceCharCounts sent]
    sentsubs = charCountsSubsets sentcc
    lengBase = lengthCharCount sentcc

    helper :: [CharCount] -> CharCount -> [[Word]] -> [[Word]] -> [[Word]]
    helper subsets cc anagrams acc = case subsets of
        []        -> anagrams
        (ss:sses) -> if ss == [] then helper sses cc anagrams acc
                                 else helper sses cc anagrams' acc
        where
          s   = if subsets == [] then [] else head subsets
          ses = if subsets == [] then [] else tail subsets
          
          an = (wordAnagrams (createWord s) dict) 

          acc' = case an of 
            [] -> [[]]
            _  -> [x ++ [y] | x <- acc, y <- an]
          
          subsets' = case an of 
            []    -> ses
            (a:_) -> charCountsSubsets cc'

          cc' = case an of           
            []    -> cc
            (a:_) -> subtractCounts cc $ wordCharCounts a

          anagrams' = case acc' of 
            [[]]  -> anagrams
            (a:_) -> if lengthWords a == lengBase && an /= []
                       then anagrams ++ acc'
                       else helper subsets' cc' anagrams acc'

lengthCharCount :: CharCount -> Int
lengthCharCount cc = foldr (\a b -> a + b) 0 [n | (_,n) <- cc]

lengthWords :: [Word] -> Int
lengthWords words = length $ foldr (\a b -> a ++ b) [] words

parseString :: String -> [Word]
parseString s = readUntilSpace s [""]
  where
    readUntilSpace :: String -> [Word] -> [Word]
    readUntilSpace s acc@(a:ac) = case s of 
      []     -> acc
      (c:cs) -> if c == ' ' 
                  then readUntilSpace cs ([""] ++ acc)
                else readUntilSpace cs ([a ++ [c]] ++ ac)

printAnagrams :: [[Word]] -> IO()
printAnagrams s = putStrLn $ init $ unlines $ printSentence s []
  where
    printSentence :: [[Word]] -> [String] -> [String]
    printSentence s acc = case s of 
      []       -> acc
      (s':ss') -> printSentence ss' (acc ++ [foldr (\a b -> a ++ " " ++ b) "" [a | a <- s']])

main = do args <- getArgs
          let str = head args
          let sentence = parseString str