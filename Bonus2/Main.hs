{- @Author
   Student Name: Ugurcan Polat
   Student ID : 150140117
   Date: 09.04.2018
-}

data Color = Red | Black
  deriving (Show)
  
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Show)
  
data Rank = Num Int | Jack | Queen | King | Ace
  deriving (Show)
  
data Card = Card { suit :: Suit, rank :: Rank }
  deriving (Show)
  
data Move = Draw | Discard Card

cardColor :: Card -> Color
cardColor (Card suit _) = case suit of 
  Clubs    -> Black
  Spades   -> Black
  Diamonds -> Red
  Hearts   -> Red
  
cardValue :: Card -> Int
cardValue (Card _ rank) = case rank of
  Num n -> n
  Ace   -> 11
  _     -> 10
