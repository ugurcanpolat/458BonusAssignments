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

data Trie = Trie {end :: Bool, children :: M.Map Char Trie} deriving (Eq)
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
  | c `elem` "aA" = Add w
  | c `elem` "sS" = Search w
  | c `elem` "fF" = Find w
  | c `elem` "pP" = Print
  | c `elem` "eE" = Exit
  | otherwise = error "Action is not defined!"

getInput :: IO Action
getInput = do putStrLn "a) Add Word"
              putStrLn "s) Search Word"
              putStrLn "f) Find words with prefix"
              putStrLn "p) Print all words"
              putStrLn "e) Exit"
              putStrLn "Enter the action:"
              a <- getLine
              let c = if null a then 'e' else head a -- 'e' means exit
              if c `elem` "aA" || c `elem` "sS" || c `elem` "fF" 
                then do putStrLn "Enter word/prefix:"
                        wp <- getLine
                        return (convertAction c wp)
                else return (convertAction c [])

doAction :: Action -> Trie -> IO Trie
doAction a t = case a of 
    Add w    -> do putStrLn "New word is added!\n" 
                   return $ insert w t
    Search w -> if search w t 
                  then do putStrLn "Exists in dictionary!\n" 
                          return t
                  else do putStrLn "NOT exist!\n"
                          return t
    Find w   -> case prefix w t of 
                  Nothing -> do putStrLn "No words found with that prefix!\n"
                                return t
                  Just ws -> do putStrLn "Found words:"
                                printWords ws
                                return t
    Print    -> do putStrLn "List of words in disctionary:"
                   printWords $ getWords t
                   return t
    Exit     -> return empty  
  where
    printWords :: [Word] -> IO ()
    printWords = putStrLn . unlines

runProgram :: Trie -> IO ()
runProgram t = if t == empty 
                 then return ()
                 else do action <- getInput
                         t' <- doAction action t
                         runProgram t'

main = do args <- getArgs
          let fileName = head args
          file <- readFile fileName
          let w = lines file -- words
          let trie = insertList w
          runProgram trie