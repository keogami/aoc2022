import System.IO
import Data.Strings

data Hand = Rock | Paper | Scissor deriving (Eq)

ourHand :: String -> Hand
ourHand "A" = Rock
ourHand "B" = Paper
ourHand  _  = Scissor

opponentHand :: String -> Hand
opponentHand "X" = Rock
opponentHand "Y" = Paper
opponentHand  _  = Scissor

parseTurn :: (String, String) -> (Hand, Hand)
parseTurn (our, their) = (opponentHand their, ourHand our)

scoreForHand :: Hand -> Int
scoreForHand h = case h of
  Rock    -> 1
  Paper   -> 2
  Scissor -> 3

kite (a, b) choice = case choice of
  True  -> a
  False -> b

winningCase :: (Hand, Hand) -> Int
winningCase hands = case hands of
  (Rock, Scissor)  -> 6
  (Paper, Rock)    -> 6
  (Scissor, Paper) -> 6
  (theirs, ours)   -> kite (3, 0) ((==) ours theirs)

score :: (Hand, Hand) -> Int
score hand = (+) (winningCase hand) (scoreForHand $ fst hand)

main = do
  content <- readFile "input/2.txt"
  let res = map parseTurn $ map (strSplit " ") $ lines content;
  print $ foldl (+) 0 $ map score res
