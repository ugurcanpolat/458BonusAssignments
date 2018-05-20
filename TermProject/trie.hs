{- @Author
   Student Name: Ugurcan Polat
   Student ID : 150140117
   Date: 21.05.2018
-}

import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: M.Map Char Trie} deriving (Show)
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
search = undefined

getWords :: Trie -> [Word]
getWords = undefined

prefix :: Word -> Trie -> Maybe [Word]
prefix = undefined