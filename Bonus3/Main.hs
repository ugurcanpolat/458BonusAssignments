{- @Author
   Student Name: Ugurcan Polat
   Student ID : 150140117
   Date: 04.05.2018
-}

import Data.Char 
import Data.Map
import Data.List
import System.Environment

main = do args <- getArgs
          let sentence = head args
          putStrLn sentence