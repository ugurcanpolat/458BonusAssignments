{- @Author
   Student Name: Ugurcan Polat
   Student ID : 150140117
   Date: 09.04.2018
-}

data Color = Red | Black
  deriving (Show, Eq)
  
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Show, Eq)
  
data Rank = Num Int | Jack | Queen | King | Ace
  deriving (Show, Eq)
  
data Card = Card { suit :: Suit, rank :: Rank }
  deriving (Show, Eq)
  
data Move = Draw | Discard Card

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
    rC [] _ _ = error "Card not found"
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
