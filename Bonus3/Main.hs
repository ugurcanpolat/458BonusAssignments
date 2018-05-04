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

{- Example Call: wordCharCounts word 
                 wordCharCounts "love" ~> [('e',1),('l',1),('o',1),('v',1)]
   This function returns character counts of input word.-}
wordCharCounts :: Word -> CharCount
wordCharCounts = toList . fromListWith (+) . map (\c -> (head c, length c)) . group . map toLower

{- Example Call: sentenceCharCounts sentence
                 sentenceCharCounts ["i","love","you"]
                 ~> [[('i',1)],[('e',1),('l',1),('o',1),('v',1)],[('o',1),('u',1),('y',1)]]
   |sentence| is a list of words contains words of given sentence.
   This function returns a list that hold character counts of input words. -}
sentenceCharCounts :: [Word] -> [CharCount]
sentenceCharCounts = map wordCharCounts

{- Example Call: dictCharCounts dictWs
                 dictCharCounts ["pea","ape"] 
                         ~> [("pea",[('a',1),('e',1),('p',1)]),("ape",[('a',1),('e',1),('p',1)])]
   |dictWs| is list of words contains words of dictionary text file.
   This function returns list of (Word,CharCount) pairs that hold 
   character counts per word. -}
dictCharCounts :: [Word] -> [(Word,CharCount)]
dictCharCounts = map (\w -> (w, wordCharCounts w))

{- Example Call: dictWordsByCharCounts wordCharPairs
                 dictWordsByCharCounts [("pea",[('a',1),('p',1),('e',1)]),("ape",[('a',1),('p',1),('e',1)])]
                                              ~> [([('a',1),('p',1),('e',1)],["ape","pea"])]
   |wordCharPairs| is list of pairs returned by dictCharCounts function.
   This function returns list of (CharCount,[Word]) pairs that combines 
   words with same character counts. -}
dictWordsByCharCounts :: [(Word,CharCount)] -> [(CharCount,[Word])]
dictWordsByCharCounts = toList . fromListWith (++) . map (\(w,cc) -> (cc, [w]))

{- Example Call: wordAnagrams word dictionary
                 wordAnagrams "pea" [([('a',1),('p',1),('e',1)],["ape"])] ~> ["ape"]
   |word| is the word wanted to find anagrams in dictionary.
   |dictionary| is list of (CharCount,[Word]) pairs returned by dictWordsByCharCounts.
   function. This function returns list of words which contains anagrams of input word |word|. -}
wordAnagrams :: Word -> [(CharCount,[Word])] -> [Word]
wordAnagrams w ccs = case result of 
    [] -> []
    _  -> head result -- Extract one element of list
  where
    comp = sortBy (\(a,_) (b,_) -> compare a b) $ wordCharCounts w -- Compare
    result = [ws | (cc,ws) <- ccs, sortBy (\(a,_) (b,_) -> compare a b) cc == comp] -- Find anagrams

{- Example Call: charCountsSubsets cc
                 charCountsSubsets [('o',1),('k',1)] ~> [[('k',1),('o',1)],[('o',1)],[('k',1)],[]]
   |cc| is list of (Char,Int) pairs returned by wordCharCounts function.
   This function returns list of character counts which is power set of gicen character counts. -}
charCountsSubsets :: CharCount -> [CharCount]
charCountsSubsets = map wordCharCounts . wordSubset . createWord
  where
    wordSubset :: Word -> [Word]
    wordSubset [] = [[]]
    wordSubset (c:cs) = nub ([c:cs' | cs' <- wordSubset cs] ++ wordSubset cs)

{- Example Call: createWord cc
                 createWord [('a',1),('p',2),('l',1),('e',1)] ~> "apple"
   |cc| is list of (Char,Int) pairs returned by wordCharCounts function.
   This function returns a word created by characters in |cc|. -}
createWord :: CharCount -> Word 
createWord cc = foldr (\a b -> a ++ b) "" [replicate n c | (c,n) <- cc]

{- Example Call: subtractCounts cc1 cc2
                 subtractCounts [('a',1),('p',2),('l',1),('e',1)] [('p',1),('e',1),('a',1)] ~> [('l',1),('p',1)]
   |cc1| and |cc2| are lists of (Char,Int) pairs returned by wordCharCounts function.
   This function returns character counts calculated by |cc1| - |cc2| -}
subtractCounts :: CharCount -> CharCount -> CharCount
subtractCounts a b = [(c,abs n) | (c,n) <- toList (fromListWith (-) (a ++ b)), n /= 0]

{- Example Call: sentenceAnagrams sentence dictionary
                 sentenceAnagrams ["live"] [([('l',1),('i',1),('v',1),('e',1)],["evil","veil"])] ~> ["evil","veil"]
   |sentence| is list of words contains words of a given sentence.
   |dictionary| is list of (CharCount,[Word]) pairs returned by dictWordsByCharCounts.
   This function returns list of sentences that are anagrams of given sentence |sentence|. -}
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

{- Example Call: lengthCharCount cc
                 lengthCharCount [('a',1),('p',2),('l',1),('e',1)] ~> 5  
   |cc| is list of (Char,Int) pairs returned by wordCharCounts function.
   This function returns number of total characters in |cc| -}
lengthCharCount :: CharCount -> Int
lengthCharCount cc = foldr (\a b -> a + b) 0 [n | (_,n) <- cc]

{- Example Call: lengthWords words
                 lengthWords ["love"] ~> 4 
   |words| is list of words, possible anagram sentence.
   This function returns number of total characters in sentence |words| -}
lengthWords :: [Word] -> Int
lengthWords words = length $ foldr (\a b -> a ++ b) [] words

{- Example Call: parseString str
                 parseString "I love you" ~> ["you","love","I"]
   |str| is a input string from main function.
   This function returns list of words of given |sentence| -}
parseString :: String -> [Word]
parseString s = readUntilSpace s [""]
  where
    readUntilSpace :: String -> [Word] -> [Word]
    readUntilSpace s acc@(a:ac) = case s of 
      []     -> acc
      (c:cs) -> if c == ' ' 
                  then readUntilSpace cs ([""] ++ acc)
                  else readUntilSpace cs ([a ++ [c]] ++ ac)

{- Example Call: printAnagrams anagrams
                 printAnagrams [["you","olive"],["olive","you"]]
                     ~> you olive
                        olive you 
   |anagrams| is list of sentences which are anagrams. 
   This function prints anagrams with proper style -}
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
          let d = lines file -- dictionary words
          let sentence = parseString str
          let dictionary = dictWordsByCharCounts $ dictCharCounts d
          let anagrams = sentenceAnagrams sentence dictionary
          printAnagrams anagrams