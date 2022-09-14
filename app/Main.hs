module Main where

import Game ( generateDeck, allCards, blackJack, calculatePoint )
import Types ( Card(Back, Blank) )
import CardPrinting ( gameHeader, showCards )
import Control.Concurrent ( threadDelay )

main :: IO ()
main = do
  -- Get shuffled deck
  deck <- generateDeck [] allCards
  gameStart deck
  -- hitDecision count deck
  threadDelay 1000000

  let dealerInitialHand = [Back, deck !! 2]
      playerInitialHand = [deck !! 1, deck !! 3]

  game deck dealerInitialHand playerInitialHand

  where
    game :: [Card] -> [Card] -> [Card] -> IO ()
    game deck dealer@(_:ds) player = do

    -- 1. Check if player bursted
      if calculatePoint player > 21 then (do
        putStrLn "You burst! Dealer wins.")
      else (do
        putStrLn $ "You now have " ++ show (calculatePoint player) ++ " points, get extra card? (y/n)"

    -- 2. If not bursted, ask if want to get extra card
        ans <- getLine
        let stop = ans /= "y"

    -- 3. If player don't want to get extra card
        if stop then (do

    -- 4. Start checking dealer's hand
          dealerFinalHand <- checkDealer deck (head deck: ds) player

    -- 9. Comparing the hands between dealer and player
          compareHands dealerFinalHand player
          )

        else (do
          let nextCard = deck !! (length dealer + length player)
          showBoard dealer (player ++ [nextCard])
          game deck dealer (player ++ [nextCard])
          )
        )

    game _ _ _ = putStrLn "Invalid game"

checkDealer :: [Card] -> [Card] -> [Card] -> IO [Card]
checkDealer deck dealer player = do

-- 5. Reveal dealer's hidden card
  showBoard dealer player
  continueDraw deck (calculatePoint dealer >= 16) dealer player

continueDraw :: [Card] -> Bool -> [Card] -> [Card] -> IO [Card]
continueDraw deck stop dealer player

-- 6. Dealer stops drawing card if the point is >= 16
  | stop = do
    let point = calculatePoint dealer
    putStrLn $ "Dealer got " ++ show point ++ " points"

-- 7. Return dealer's final hand
    return dealer

-- 8. Dealer continue to draw if the point is smaller than 16
  | otherwise = do
    let nextCard = deck !! (length dealer + length player)
    showBoard (dealer ++ [nextCard]) player
    continueDraw deck (calculatePoint (dealer ++ [nextCard]) >= 16) (dealer ++ [nextCard]) player

-- 11. End game checking
compareHands :: [Card] -> [Card] -> IO ()
compareHands dealer player
  | calculatePoint dealer > 21 = putStrLn "Dealer bursts! You win."
  | blackJack dealer = putStrLn "Dealer got BlackJack! You lose!"
  | blackJack player = putStrLn "Congradualation! You win with a BlackJack!"
  | otherwise = if calculatePoint player > calculatePoint dealer
    then putStrLn "Congradulations! You win!"
    else putStrLn "You lose!"

gameStart :: [Card] -> IO ()
gameStart d = do
  putStrLn ""
  putStrLn "Game Started!"
  putStrLn ""

  showBoard [Back] [Blank]
  showBoard [Back] [d !! 1]
  showBoard [Back, d !! 2] [d !! 1]
  showBoard [Back, d !! 2] [d !! 1, d !! 3]

showBoard :: [Card] -> [Card] -> IO ()
showBoard dealer player = do
  threadDelay 1000000
  gameHeader dealer player
  showCards True dealer
  showCards False player
