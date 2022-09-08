module Types where

data Suit = Diamond | Club | Heart | Spade deriving (Show, Enum, Eq)
data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
  deriving (Show, Enum, Eq)

data Card = Blank | Back | Card Suit Rank deriving (Show, Eq)