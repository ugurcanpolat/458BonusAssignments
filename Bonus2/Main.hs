{- @Author
   Student Name: Ugurcan Polat
   Student ID : 150140117
   Date: 09.04.2018
-}

import Data.Char -- eq for chars

data Color = Red | Black
  deriving (Show, Eq) -- show for debugging

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Show, Eq) -- show for debugging

data Rank = Num Int | Jack | Queen | King | Ace
  deriving (Show, Eq) -- show for debugging

data Card = Card { suit :: Suit, rank :: Rank }
  deriving (Show, Eq) -- show for debugging

data Move = Draw | Discard Card
  deriving (Show, Eq) -- show for debugging

cardColor :: Card -> Color
cardColor (Card suit _) = case suit of 
  Clubs    -> Black
  Spades   -> Black
  _        -> Red

cardValue :: Card -> Int
cardValue (Card _ rank) = case rank of
  Num n -> n
  Ace   -> 11
  _     -> 10

removeCard :: [Card] -> Card -> [Card]
removeCard cs c = rC cs c []
  where
    rC :: [Card] -> Card -> [Card] -> [Card]
    rC [] _ _ = error "card not in list"
    rC (c':cs') c head
      | c' == c   = head ++ cs'
      | otherwise = rC cs' c (head ++ [c'])

allSameColor :: [Card] -> Bool
allSameColor cs = case cs of
  []           -> True
  [_]          -> True
  c1:cs@(c2:_) -> cardColor c1 == cardColor c2 && allSameColor cs

sumCards :: [Card] -> Int
sumCards cs = sum' cs 0 
  where 
    sum' cs acc = case cs of 
      []    -> acc
      c:cs' -> sum' cs' (acc + cardValue c)

score :: [Card] -> Int -> Int
score cs g 
  | allSameColor cs = floor (fromIntegral pre / 2.0)
  | otherwise       = pre
  where
    pre
      | sum > g   = 3 * (sum - g)
      | otherwise = g - sum
    sum = sumCards cs

type State = ([Card],[Card]) -- List of cards and held-cards

runGame :: [Card] -> [Move] -> Int -> Int
runGame cs ms g = helper (cs, []) ms g
  where
    helper :: State -> [Move] -> Int -> Int
    helper (cs,hs) ms g = case ms of 
      []       -> score hs g
      (m':ms') -> case (makeMove (cs,hs) m' g) of
        ([],[]) -> score hs g
        _       -> helper (makeMove (cs,hs) m' g) ms' g
      where
        makeMove :: State -> Move -> Int -> State -- find next state
        makeMove (cs,hs) m g = case m of
          Draw      -> if cs == [] || sumCards (c':hs) > g 
                         then ([],[]) -- game is over
                         else (cs',(c':hs)) -- next state
          Discard c -> (cs,(removeCard hs c)) -- next state
          where
            c'  = head cs
            cs' = tail cs

-- Part-2

convertSuit :: Char -> Suit
convertSuit c
  | c == 'c' || c == 'C' = Clubs
  | c == 'd' || c == 'D' = Diamonds
  | c == 'h' || c == 'H' = Hearts
  | c == 's' || c == 'S' = Spades
  | otherwise            = error "suit is unknown"

convertRank :: Char -> Rank
convertRank c 
  | isDigit c = digit c
  | otherwise = chr c
  where
    digit :: Char -> Rank
    digit c 
      | d == 1    = Ace
      | otherwise = Num d
      where 
        d = digitToInt c
        
    chr :: Char -> Rank
    chr c
      | c == 't' || c == 'T'  = Num 10
      | c == 'j' || c == 'J'  = Jack
      | c == 'q' || c == 'Q'  = Queen
      | c == 'k' || c == 'K'  = King
      | otherwise = error "rank is unknown"

convertCard :: Char -> Char -> Card
convertCard s r = Card {suit = s', rank = r'}
  where
    s' = convertSuit s
    r' = convertRank r

readCards :: IO [Card]
readCards = helper []
  where
    helper cards = do line <- getLine
                      if line == "."
                         then return cards
                         else helper (cards ++ [parse line])
      where
        parse :: String -> Card
        parse l = case l of 
          [s,r]     -> convertCard s r
          otherwise -> error "wrong input"

convertMove :: Char -> Char -> Char -> Move
convertMove m s r 
  | m == 'd' || m == 'D' = Draw
  | m == 'r' || m == 'R' = Discard (convertCard s r)
  | otherwise            = error "wrong move"

readMoves :: IO [Move]
readMoves = helper [] 
  where
    helper moves = do line <- getLine
                      if line == "."
                         then return moves
                         else helper (moves ++ [parse line])
      where
        parse :: String -> Move
        parse l = case l of
          [m]       -> convertMove m ' ' ' '
          [m,s,r]   -> convertMove m s r
          otherwise -> error "wrong input"

main = do putStrLn "Enter cards:"
          cards <- readCards
          -- putStrLn (show cards)
          putStrLn "Enter moves:"
          moves <- readMoves
          -- putStrLn (show moves)
          putStrLn "Enter goal:"
          line <- getLine
          let goal = read line :: Int
          let score = runGame cards moves goal
          putStrLn ("Score: " ++ show score)