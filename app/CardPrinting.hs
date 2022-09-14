-- Handles printing effects
module CardPrinting where

import Types
import Game

gameHeader :: [Card] -> [Card] -> IO ()
gameHeader dealer player = do
  putStrLn ""
  putStrLn "Points"
  putStrLn $ "Dealer's : " ++ show (calculatePoint dealer)
  putStrLn $ "Your     : " ++ show (calculatePoint player)


showCards :: Bool -> [Card] -> IO ()
showCards dealer cardToShow = do
  putStrLn $ headlineBlank ++ cardsLine1 cardToShow
  putStrLn $ headlineBlank ++ cardsLine2 cardToShow
  putStrLn $ headlineName ++ cardsSuitLine cardToShow
  putStrLn $ headlineHand ++ cardsRankLine cardToShow
  putStrLn $ headlineBlank ++ cardsLine2 cardToShow
  putStrLn $ headlineBlank ++ cardsLine1 cardToShow
    where
      headlineBlank = "          | "
      headlineName = if dealer then " Dealer's | " else "   Your   | "
      headlineHand = "   Hand   | "

cardsLine1 :: [Card] -> String
cardsLine1 [] = ""
cardsLine1 (c:cs) = cardLine1 c ++ " " ++ cardsLine1 cs
  where
    cardLine1 :: Card -> String
    cardLine1 cl1
      | cl1 == Blank = "           "
      | otherwise  = "  -------  "

cardsLine2 :: [Card] -> String
cardsLine2 [] = ""
cardsLine2 (c:cs) = cardLine2 c ++ " " ++ cardsLine2 cs
  where
    cardLine2 :: Card -> String
    cardLine2 cl2
      | cl2 == Blank = "           "
      | otherwise    = " |       | "

cardsSuitLine :: [Card] -> String
cardsSuitLine [] = ""
cardsSuitLine (c:cs) = suitLine c ++ " " ++ cardsSuitLine cs
  where
    suitLine :: Card -> String
    suitLine scl = case scl of
      Card Diamond _ -> " |" ++ show Diamond ++ "| "
      Card Club _    -> " | " ++ show Club ++ "  | "
      Card Heart _   -> " | " ++ show Heart ++ " | "
      Card Spade _   -> " | " ++ show Spade ++ " | "
      Blank          -> "           "
      Back           -> " |       | "

cardsRankLine :: [Card] -> String
cardsRankLine [] = ""
cardsRankLine (c:cs) = rankLine c ++ " " ++ cardsRankLine cs
  where
    rankLine :: Card -> String
    rankLine rcl = case rcl of
      Card _ rll -> case length $ show rll of
        3 -> " |  " ++ show rll ++ "  | "
        4 -> " | " ++ show rll ++ "  | "
        _ -> " | " ++ show rll ++ " | "
      Blank          -> "           "
      Back           -> " |       | "