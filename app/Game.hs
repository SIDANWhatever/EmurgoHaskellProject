module Game where

import System.Random (randomRIO)
import Types

-- Game setting logic: To generate a shuffled deck below
--                     using `generateDeck [] allCards`

generateDeck :: [Card] -> [Card] -> IO [Card]
generateDeck d []           = return d
generateDeck d toBeShuffled = do
  index <- randomRIO (0, length toBeShuffled - 1)
  let card = toBeShuffled !! index
  generateDeck (card:d) (removeItem card toBeShuffled)

removeItem :: Card -> [Card] -> [Card]
removeItem _ [] = []
removeItem card (d:ds)
  | card == d = ds
  | otherwise = d : removeItem card ds

allCards :: [Card]
allCards = getAllCards [Diamond, Club, Heart, Spade] [Ace , Two , Three , Four , Five , Six , Seven , Eight , Nine , Ten , Jack , Queen , King]

getAllCards :: [Suit] -> [Rank] -> [Card]
getAllCards [] _ = []
getAllCards (x:xs) r = cards x r ++ getAllCards xs r

cards :: Suit -> [Rank] -> [Card]
cards = map . Card


-- Actual game logic

blackJack :: [Card] -> Bool
blackJack [c1,c2] = case (c1,c2) of
  (Card _ Ace, Card _ o) -> o == Jack || o == Queen || o == King
  (Card _ o, Card _ Ace) -> o == Jack || o == Queen || o == King 
  _ -> False
blackJack _       = False

calculatePoint :: [Card] -> Int
calculatePoint [] = 0
calculatePoint hand = sortHighestValidPoint (getPotentialPointList hand [0]) 0
  where
    getPotentialPointList :: [Card] -> [Int] -> [Int]
    getPotentialPointList [] points = points
    getPotentialPointList (c:cs) points
      | cardPoint c == 1 = getPotentialPointList cs (map (+1) points ++ map (+11) points)
      | otherwise        = getPotentialPointList cs (map (+ cardPoint c) points)

    sortHighestValidPoint :: [Int] -> Int -> Int
    sortHighestValidPoint [] x = x
    sortHighestValidPoint (p:ps) x
      | x == 0           = sortHighestValidPoint ps p
      | p > x && p <= 21 = sortHighestValidPoint ps p
      | p < x && x >  21 = sortHighestValidPoint ps p
      | otherwise        = sortHighestValidPoint ps x


cardPoint :: Card -> Int
cardPoint (Card _ cardRank)
  | fromEnum cardRank >= 10 = 10
  | otherwise               = fromEnum cardRank + 1
cardPoint _ = 0

-- Other functions for testing

suitRangeRand :: (Int, Int)
suitRangeRand = (0,3)

rankRangeRand :: (Int, Int)
rankRangeRand = (0,12)

randomDrawOneCard :: IO Card
randomDrawOneCard = do
  suitEnum <- randomRIO suitRangeRand
  rankEnum <- randomRIO rankRangeRand
  let suit = toEnum suitEnum :: Suit
      rank = toEnum rankEnum :: Rank
  return $ Card suit rank

usedCardsEg :: [Card]
usedCardsEg = getAllCards [Diamond, Club, Heart, Spade] [Ace , Two , Three , Four , Five , Six , Seven , Eight , Nine , Ten , Jack , Queen ]
