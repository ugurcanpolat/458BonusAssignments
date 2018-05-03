{- @Author
   Student Name: Ugurcan Polat
   Student ID : 150140117
   Date: 04.05.2018
-}

import Prelude hiding (Word)
import Data.Char 
import Data.Map hiding (map,foldr)
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
    _  -> head result -- Extract one element of list
  where
    comp = sortBy (\(a,_) (b,_) -> compare a b) $ wordCharCounts w -- Compare
    result = [ws | (cc,ws) <- ccs, sortBy (\(a,_) (b,_) -> compare a b) cc == comp] -- Find anagrams

charCountsSubsets :: CharCount -> [CharCount]
charCountsSubsets = map wordCharCounts . wordSubset . createWord
  where
    wordSubset :: Word -> [Word]
    wordSubset [] = [[]]
    wordSubset (c:cs) = nub ([c:cs' | cs' <- wordSubset cs] ++ wordSubset cs)

-- Create word from char counts
createWord :: CharCount -> Word 
createWord cc = foldr (\a b -> a ++ b) "" [replicate n c | (c,n) <- cc]

subtractCounts :: CharCount -> CharCount -> CharCount
subtractCounts a b = [(c,abs n) | (c,n) <- toList (fromListWith (-) (a ++ b)), n /= 0]

sentenceAnagrams :: [Word] -> [(CharCount,[Word])] -> [[Word]]
sentenceAnagrams sent dict = helper sentsubs sentcc [] [[]]
  where
    sentcc   = toList $ fromListWith (+) $ foldr (\a b -> a ++ b) [] [a | a <- sentenceCharCounts sent] -- Char count
    sentsubs = charCountsSubsets sentcc -- Base subsets of char counts
    lengBase = lengthCharCount sentcc -- Goal anagram length

    helper :: [CharCount] -> CharCount -> [[Word]] -> [[Word]] -> [[Word]]
    helper subsets cc anagrams acc = case subsets of
        []        -> anagrams -- All subsets are iterated
        (ss:sses) -> if ss == [] then helper sses cc anagrams acc -- Empty subset
                                 else helper sses cc anagrams' acc -- Iterate over subsets
        where
          s   = if subsets == [] then [] else head subsets -- Check if subsets empty
          ses = if subsets == [] then [] else tail subsets -- Check if subsets empty
          
          an = (wordAnagrams (createWord s) dict) -- Anagram of a subset

          acc' = case an of -- Accumulator of anagram of words
            [] -> [[]]
            _  -> [x ++ [y] | x <- acc, y <- an]
          
          subsets' = case an of -- Current subsets
            []    -> ses
            (a:_) -> charCountsSubsets cc'

          cc' = case an of -- Remaining character counts        
            []    -> cc
            (a:_) -> subtractCounts cc $ wordCharCounts a

          anagrams' = case acc' of -- New anagram case
            [[]]  -> anagrams
            (a:_) -> if lengthWords a == lengBase && an /= [] -- Anagram of sentece found
                       then anagrams ++ acc'
                       else helper subsets' cc' anagrams acc' -- Anagram word found iterate to find sentence

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
printAnagrams s = putStrLn $ init $ unlines str
  where
    str = case s of 
      [] -> ["*** No anagram found in the dictionary ***"] -- Empty anagram case
      _  -> printSentence s []

    -- Combine all anagram words in list of string 
    printSentence :: [[Word]] -> [String] -> [String]
    printSentence s acc = case s of 
      []       -> acc
      (s':ss') -> printSentence ss' (acc ++ [foldr (\a b -> a ++ " " ++ b) "" [a | a <- s']])

main = do args <- getArgs
          let str = head args
          file <- readFile "words.txt"
          let d = lines file
          let sentence = parseString str
          let dictionary = dictWordsByCharCounts $ dictCharCounts d
          let anagrams = sentenceAnagrams sentence dictionary
          printAnagrams anagrams