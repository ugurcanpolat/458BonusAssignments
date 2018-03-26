{- @Author
   Student Name: Ugurcan Polat
   Student ID : 150140117
   Date: 26.02.2018
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
