module Blackjack where
import System.Random (randomRIO)
import Data.List (partition, sort)

data Card = Card Value Suit deriving (Show, Ord, Eq)
data Suit = Spades | Clubs | Hearts | Diamonds deriving (Show, Enum, Ord, Eq)
data Value = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Eq, Enum, Ord)

data Player = Player [Card] | Dealer [Card] deriving Show
--
--class Dealable a where
--  dealCard :: Card -> a -> a
--  cards :: a -> [Card]
--
--instance Dealable (Dealer) where
--  dealCard c (Dealer cs) = Dealer (c:cs)
--  cards (Dealer cs) = cs
--
--instance Dealable (Player) where
--  dealCard c (Player cs) = Player (c:cs)
--  cards (Player cs) = cs
--

dealCard c (Player cs) = Player (c:cs)
dealCard c (Dealer cs) = Dealer (c:cs)

cards (Player cs) = cs
cards (Dealer cs) = cs

deal :: [Card] -> [Player] -> ([Card], [Player])
deal deck (p:ps) | length (cards p) == 2 = (deck, (p:ps))
                 | otherwise             = (tail deck, ps ++ [(dealCard (head deck) p)])

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

value :: Card -> Value
value (Card v _) = v

optimalValueOfCards :: [Card] -> Int
optimalValueOfCards cs = sumOfOthers + (optimumValueGivenAces aces sumOfOthers)
                         where
                         (aces, others) = partition isAce cs
                         sumOfOthers    = sum $ map valueOfCard others

valueOfCard :: Card -> Int
valueOfCard c = case lookup (value c) (zip [Ace .. King] ([1..10] ++ [10,10,10])) of
                  Nothing -> error "Card not found."
                  Just n  -> n

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

