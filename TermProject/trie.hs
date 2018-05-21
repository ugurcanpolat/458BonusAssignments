{- @Author
   Student Name: Ugurcan Polat
   Student ID : 150140117
   Date: 22.05.2018
-}

import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: M.Map Char Trie} deriving (Eq,Show)
type Word = String

empty :: Trie
empty = Trie {end = False, children = M.empty}

insert :: Word -> Trie -> Trie
insert [] (Trie _ m)     = Trie True m
insert (c:cs) (Trie e m) = case M.lookup c m of
    Nothing -> Trie e $ M.insert c (insert cs empty) m
    Just t  -> Trie e $ M.insert c (insert cs t) m

insertList :: [Word] -> Trie
insertList = foldr insert empty 

search :: Word -> Trie -> Bool
search [] (Trie e _)     = e
search (c:cs) (Trie _ m) = case M.lookup c m of 
    Nothing -> False
    Just t  -> search cs t

getWords :: Trie -> [Word]
getWords (Trie _ m) = traverser (M.toList m) [] []
  where
    traverser :: [(Char,Trie)] -> [Word] -> [Word] -> [Word]
    traverser [] _ acc = acc
    traverser l wAcc acc
      | e == True = if M.null m' == True
                       then traverser ts ws (acc ++ [w])
                       else traverser newList ((replicate size w) ++ ws) (acc ++ [w])
      | otherwise = traverser newList ((replicate size w) ++ ws) acc
        where
            t  = head l
            ts = if null l then [] else tail l 
            c  = fst t
            tr = snd t

            getMap (Trie _ m) = m
            getEnd (Trie e _) = e

            e  = getEnd tr
            m' = getMap tr

            size = M.size m'
            newList = M.toList m' ++ ts
            w   = if null wAcc then [c] else head wAcc ++ [c]
            ws  = if null wAcc then [] else tail wAcc

prefix :: Word -> Trie -> Maybe [Word]
prefix w t = if null found then Nothing else Just found
  where
    ws    = getWords t
    found = [x | x <- ws, w == take (length w) x, w /= x]

data Action = Add Word | Search Word | Find Word | Print | Exit

convertAction :: Char -> Word -> Action
convertAction c w 
  | c == 'a' || c == 'A' = Add w
  | c == 's' || c == 'S' = Search w
  | c == 'f' || c == 'F' = Find w
  | c == 'p' || c == 'P' = Print
  | c == 'e' || c == 'E' = Exit
  | otherwise = error "Action is not defined!"