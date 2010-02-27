module Blackjack where
import System.Exit
import System.Random (randomRIO)
import Data.List (partition, sort, intercalate)
import Control.Monad (forM_, foldM)

data Card = Card Value Suit deriving (Ord, Eq)
data Suit = Spades | Clubs | Hearts | Diamonds deriving (Show, Enum, Ord, Eq)
data Value = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Eq, Enum, Ord)

data Player = Player [Card] | Dealer [Card] deriving Show

instance Show Card where
  show c@(Card v s) = colorForCard c (concat [show v, " of ", show s])

value :: Card -> Value
value (Card v _) = v

suit :: Card -> Suit
suit (Card _ s) = s

deal :: [Card] -> [Player] -> ([Card], [Player])
deal deck (p:ps) | length (cards p) == 2 = (deck, (p:ps))
                 | otherwise             = (d, ps')
                 where
                 (d, ps') = deal (tail deck) (ps ++ [(dealCard (head deck) p)])

freshDeck :: [Card]
freshDeck = [Card v s | s <- [Spades .. Diamonds], v <- [Ace .. King]]

shuffle :: [Card] -> IO [Card]
shuffle [] = return []
shuffle cs = do r <- randomRIO (0, length cs - 1)
                case removeAt r cs of
                  (Nothing, cs') -> return cs' -- something has gone wrong
                  (Just c, cs')   -> shuffle cs' >>= (return . (c:))

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ []     = (Nothing, [])
removeAt 0 (x:xs) = (Just x, xs)
removeAt i (x:xs) = (y, x:ys)
                    where
                    (y, ys) = removeAt (i - 1) xs

main :: IO ()
main = menu

prompt :: String
prompt = "> "

clearScreen :: IO ()
clearScreen = putStrLn "\ESC[2J\ESC[H"

ansiColour :: String -> String -> String
ansiColour code str = concat ["\ESC[", code, "m", str, "\ESC[0m"]

ansiRed :: String -> String
ansiRed = ansiColour "31"

ansiBlack :: String -> String
ansiBlack = ansiColour "30"

colorForCard :: Card -> (String -> String)
colorForCard c = colorOfSuit (suit c)

colorOfSuit :: Suit -> (String -> String)
colorOfSuit Diamonds = ansiRed
colorOfSuit Hearts   = ansiRed
colorOfSuit Spades   = ansiBlack
colorOfSuit Clubs    = ansiBlack

menu :: IO ()
menu = do clearScreen
          putStrLn "Welcome to Blackjack! Press 'n' to start a new game.\n\n"
          putStrLn "What do you want to do? (press 'h' for help)"
          putStr prompt
          input <- getLine
          case input of
            "n" -> newGame
            "h" -> menuHelpScreen >> menu
            "q" -> quitGame
            _   -> menuHelpScreen >> menu
          return ()

newGame = do clearScreen
             d <- shuffle freshDeck
             let (deck, [player, dealer]) = deal d [Player [], Dealer []]
             (deck', dealer', players) <- play deck dealer [player]
             printGameResults dealer' players

play :: [Card] -> Player -> [Player] -> IO ([Card], Player, [Player])
play deck dealer players = do (cs, ps) <- playPlayers deck dealer players
                              let (cs', d) = playDealerHand cs dealer
                              return (cs', d, ps)

playPlayers :: [Card] -> Player -> [Player] -> IO ([Card], [Player])
playPlayers deck dealer players = foldM f (deck, []) players
                                  where
                                  f :: ([Card], [Player]) -> Player -> IO ([Card], [Player])
                                  f (cs, ps) p = do (cs', p') <- playHand cs dealer p
                                                    return (cs', (p':ps))

printGameResults :: Player -> [Player] -> IO ()
printGameResults dealer players = mapM (printResult dealer) players >> return ()

printResult :: Player -> Player -> IO ()
printResult dealer player = case winner dealer player of
                              (Dealer _) -> putStrLn "Dealer wins!"
                              _          -> putStrLn "Player wins!"

printDealersFaceUpCard :: Player -> IO ()
printDealersFaceUpCard dealer = putStrLn $ "Dealer: " ++ (show . head . cards) dealer

printPlayersCards :: Player -> IO ()
printPlayersCards player = putStrLn $ "You: " ++ displayStringForCards (cards player)

playDealerHand :: [Card] -> Player -> ([Card], Player)
playDealerHand deck dealer = if optimalValueOfCards (cards dealer) < 17 then (deck, dealer) else playDealerHand (tail deck) (dealCard (head deck) dealer)

playHand :: [Card] -> Player -> Player -> IO ([Card], Player)
playHand deck dealer player = do clearScreen
                                 printDealersFaceUpCard dealer
                                 printPlayersCards player
                                 if isBust player
                                   then do putStrLn "Sorry, you have bust!"
                                           return (deck, player)
                                   else do putStrLn "What do you want to do? (press 'h' for help)"
                                           putStr prompt
                                           m <- getLine
                                           case m of
                                             "h"     -> gameHelpScreen >> playHand deck dealer player
                                             "hit"   -> playHand (tail deck) dealer (dealCard (head deck) player)
                                             "stand" -> return (deck, player)
                                             "q"     -> quitGame
                                             _       -> gameHelpScreen >> playHand deck dealer player

displayStringForCards :: [Card] -> String
displayStringForCards cs = intercalate ", " (map show cs)

gameHelpScreen :: IO ()
gameHelpScreen = helpScreen ["h\t Show this help page.",
                             "hit\t Take another card from the deck.",
                             "stand\t Keep your current cards.",
                             "q\t Quit the game."]

quitGame = do clearScreen
              putStrLn "Bye :("
              exitSuccess

menuHelpScreen = helpScreen ["h\t Show this help page.",
                             "n\t Start a new game.",
                             "q\t Quit."]

helpScreen :: [String] -> IO ()
helpScreen commands = do clearScreen
                         putStrLn . unlines $ ["Welcome to Blackjack! If you are not familiar with the rules, I recommend checking out this site:",
                                               "http://www.blackjackinfo.com/blackjack-rules.php\n",
                                               "Dealer stands on all 17s."]
                         (putStrLn . unlines . map ("\t" ++)) commands
                         continue

continue :: IO ()
continue = do putStrLn "[Press enter to continue]"
              getLine
              return ()

dealCard c (Player cs) = Player (c:cs)
dealCard c (Dealer cs) = Dealer (c:cs)

cards (Player cs) = cs
cards (Dealer cs) = cs

winner d p | dv == pv  = betterCards d p
           | dv >= pv  = d
           | otherwise = p
           where
           dv = f d
           pv = f p
           f  = optimalValueOfCards . cards

betterCards :: Player -> Player -> Player
betterCards d p = if cardsRanking d >= cardsRanking p then d else p

cardsRanking :: Player -> Int
cardsRanking x = case values (sort (cards x)) of
                   [Ace, King]           -> 3
                   [Ace, Queen]          -> 3
                   [Ace, Jack]           -> 3
                   [Ace, Ten]            -> 2
                   (_:(_:(_:(_:(_:_))))) -> 1
                   _                     -> 0

values :: [Card] -> [Value]
values cs = map value cs


isBust :: Player -> Bool
isBust = (> 21) . optimalValueOfCards . cards

optimalValueOfCards :: [Card] -> Int
optimalValueOfCards cs = optimumValueGivenAces aces sumOfOthers
                         where
                         (aces, others) = partition isAce cs
                         sumOfOthers    = sum $ map valueOfCard others

valueOfCard :: Card -> Int
valueOfCard c = maybe (error "Card not found.") id $ lookup (value c) (zip [Ace .. King] ([1..10] ++ [10,10,10]))

isAce :: Card -> Bool
isAce (Card Ace _) = True
isAce _            = False

optimumValueGivenAces :: [Card] -> Int -> Int
optimumValueGivenAces []     init = init
optimumValueGivenAces (a:as) init | as' > 21                 = as''
                                  | as'' > 21                = as'
                                  | (21 - as') < (21 - as'') = as'
                                  | otherwise                = as''
                                  where
                                  as'  = (+) init $ sum $ 11 : take (length as) (cycle [1])
                                  as'' = (+) init $ sum $ take (length (a:as)) (cycle [1])

