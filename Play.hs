-- CPSC 312 PROJECT 2 GO FISH --
-- Authors: Nafisa Shazia   18769142   
--          John Jang       52043122  
--
-- This is class is responsible for actual game play of GoFish

-- To run, in ghci
--
-- :l Play
-- goFishStart


module Play where

import GoFish           -- Actual GoFish game states
import System.IO        -- For basic I/O
import Text.Read        -- For reading/parsing inputs
import Data.Array.IO    -- For randomness (shuffling cards)
import System.Random    -- For randomness (shuffling cards, ai moves)
import Control.Monad

--type GameInfo = (AllPlayers, [Int]) --0th index is for 0th player's score
type GameInfo = ([Bool],[Int])

-- Actual start of the game. It will set all number of players/ai and new set of deck
goFishStart :: IO GameInfo
goFishStart = let
    deck = [(x,y)|x<-[Two ..], y<-[Club ..]] in -- List of all decks
    do
        putStrLn ("Welcome to GoFish in Haskell!")
        numOfPlayers <- readInt ("Enter number of human players (1~4):")
        if numOfPlayers == 1 then
            do
            numOfAI <- readInt ("How many of AI do you want? (0~" ++ show (4-numOfPlayers) ++ ")" )
            if(numOfAI < 4) then
                do
                deckS <- shuffle deck
                if numOfAI == 1 then
                    do
                        play ( (start (2+numOfAI) (7-numOfAI) deckS),True) (makeGameInfo numOfAI ([False, True],[]))
                else if numOfAI == 2 then
                    do
                        play ( (start (2+numOfAI) (7-numOfAI) deckS),True) (makeGameInfo numOfAI ([False,True,True],[]))
                else
                    do
                        play ( (start (2+numOfAI) (7-numOfAI) deckS),True) (makeGameInfo numOfAI ([False,True,True,True],[]))
            else 
                do
                putStrLn ("Not a valid range")
                goFishStart
        else if numOfPlayers == 2 then
            do
            numOfAI <- readInt ("How many of AI do you want? (0~" ++ show (4-numOfPlayers) ++ ")" )
            if(numOfAI < 3) then
                do
                if numOfAI == 1 then
                    do
                        deckS <- shuffle deck
                        play ( (start (2+numOfAI) (7-numOfAI) deckS),True) (makeGameInfo numOfAI ([False,False,True],[]))
                else
                    do
                        deckS <- shuffle deck
                        play ( (start (2+numOfAI) (7-numOfAI) deckS),True) (makeGameInfo numOfAI ([False,False,True,True],[]))
            else 
                do
                putStrLn ("Not a valid range")
                goFishStart
        else if numOfPlayers == 3 then
            do
            numOfAI <- readInt ("How many of AI do you want? (0~" ++ show (4-numOfPlayers) ++ ")" )
            if(numOfAI < 2) then
                do
                deckS <- shuffle deck
                if numOfAI == 1 then
                    do
                        play ( (start (2+numOfAI) (7-numOfAI) deckS),True) (makeGameInfo numOfAI ([False,False,False,True],[]))
                else
                    do
                        putStrLn ("Not a valid range")
                        goFishStart
            else 
                do
                putStrLn ("Not a valid range")
                goFishStart
        else if numOfPlayers == 4 then
            do
                deckS <- shuffle deck
                play ( (start (4) (5) deckS),True) (makeGameInfo 0 ([False,False,False,False],[]))
        else
            do
            putStrLn ("Invalid input\n")
            goFishStart

-- play will be going back and forth between players/ai during each state
--  Depending on each state, it will let players/ai play that respective stage 
--  When it is AI's turn, it will let AI play. When it is human's turn, it will let the player play
play :: Result -> GameInfo -> IO GameInfo
-- Initial state
play (StartS info, True) gameInfo =
    if checkIfAI info gameInfo then
        (ai_play (StartS info, True) gameInfo)
    else
        (person_play (StartS info, True) gameInfo)

-- Giving ard from A to B state
play (GiveS card info, True) gameInfo = 
    if checkIfAI info gameInfo then
      (ai_play (GiveS card info, True) gameInfo)
    else
        (person_play (GiveS card info, True) gameInfo)

-- Saying GoFish state
play (GoFishS card info, True) gameInfo =
    if checkIfAI info gameInfo then
        (ai_play (GoFishS card info, True) gameInfo)
    else
        (person_play (GoFishS card info, True) gameInfo)

-- Drawing a card state
play (DrawS info, True) gameInfo =
    if checkIfAI info gameInfo then
        (ai_play (DrawS info, True) gameInfo)
    else
        (person_play (DrawS info, True) gameInfo)

-- End of game state, it will simply print out the winner
play (EndS n, True) gameInfo =  do
    putStrLn ("Game has ended, player " ++ show (n) ++ " has won the game!")
    putStrLn ("To play again, type GoFishStart on GHCI")
    return gameInfo

----------------------------------------------------------

ai_play :: Result -> GameInfo -> IO  GameInfo
ai_play (StartS info, True) gameInfo = let
    hands' = hands $ (players info)!!(source info)
    players' = players info
    rangeC = (length $ hands $ (players info)!!(source info))-1
    rangeP = (length (players info))-1 in
    do
        indexC <- shuffle [0..rangeC]
        indexP <- shuffle [0..rangeP]
        if ( head indexP == (source info) ) then
            do
            (ai_play (StartS info, True) gameInfo)
        else
            do
            play (goFish (StartS info, Ask (head indexP) (hands'!!(head indexC)))) gameInfo

--    do
--        putStr (show (StartS info))
--        card <- readCard
--        player <- readInt "\nEnter player number to ask: " 
--        if (card == Zero || player == (source info) || player >= length (players info) 
--            || not (checkCard (card,Dummy) ((players info)!!(source info))) ) then
--            do
--            putStr ("\n---Invalid card or player number.\n")
--            (person_play (StartS info, True) gameInfo)
--        else 
--            do
--            play (goFish (StartS info, Ask player (card, Dummy))) gameInfo
    
ai_play (GiveS card info, True) gameInfo = 
    do
        putStr ("AI is making the move...Give " ++ show (card))
        putStr (show (GiveS card info))
        (play (goFish (GiveS card info, Give)) gameInfo)

ai_play (GoFishS card info, True) gameInfo = 
    do
        putStr ("AI is making the move...GoFish")
        putStr (show (GoFishS card info))
        (play (goFish (GoFishS card info, GoFish)) gameInfo)

ai_play (DrawS info, True) gameInfo = 
    do
        putStr ("AI is making the move...Draw")
        putStr (show (DrawS info))
        (play (goFish (DrawS info, Draw)) gameInfo)


person_play :: Result -> GameInfo -> IO GameInfo
-- Initial/start state for a human player. It will ask for a card 
--  and a player number to choose from.
person_play (StartS info, True) gameInfo =
    do
        putStr (show (StartS info))
        card <- readCard
        player <- readInt "\nEnter player number to ask: " 
        if (card == Zero || player == (source info) || player >= length (players info) 
            || not (checkCard (card,Dummy) ((players info)!!(source info))) ) then
            do
            putStr ("\n---Invalid card or player number.\n")
            (person_play (StartS info, True) gameInfo)
        else 
            do
            play (goFish (StartS info, Ask player (card, Dummy))) gameInfo

-- Give state for a human player
person_play (GiveS card info, True) gameInfo =
    do
        putStr (show (GiveS card info))
        move <- readMove "Enter Move: "
        if (move /= Give) then
            do
            putStr ("\n---You have the card! Don't lie!. Your only valid move is Give.\n")
            (person_play (GiveS card info, True) gameInfo)
        else
            do
            play (goFish (GiveS card info, move)) gameInfo

-- GoFish state for a human player
person_play (GoFishS card info, True) gameInfo =
    do
        putStr (show (GoFishS card info))
        move <- readMove "Enter Move: "
        if (move /= GoFish) then
            do
            putStr ("\n---You don't have the card... Your only valid move is to say GoFish.\n")
            (person_play (GoFishS card info, True) gameInfo)
        else
            do
            play (goFish (GoFishS card info, move)) gameInfo

-- Draw state for a human player
person_play (DrawS info, True) gameInfo =
    do
        putStr (show (DrawS info))
        move <- readMove "Enter Move: "
        if (move /= Draw) then
            do
            putStr ("\n---You have nothing to do except draw the card. Type Draw to draw a card.\n")
            (person_play (DrawS info, True) gameInfo)
        else
            do
            play (goFish (DrawS info, move)) gameInfo


-------------Helper function-------------

-- check if current source player is an AI or not. Return True if ai
checkIfAI :: Info -> GameInfo -> Bool
checkIfAI info gameInfo = let
    source' = source info
    lst = fst gameInfo in
    lst!!source'



-- Make ai/players
makeGameInfo :: Int -> GameInfo -> GameInfo
makeGameInfo 0 gameInfo = gameInfo
makeGameInfo num gameInfo = let
    lst = fst gameInfo 
    rest = snd gameInfo in
    makeGameInfo (num-1) ((lst++[True]),rest)

-- Read an Int from the user
readInt :: String -> IO Int
readInt msg = 
    do
        putStrLn(msg)
        hFlush stdout
        readLn

-- Read a card rank from the user
readCard :: IO Rank
readCard =
    do
        putStrLn("Enter a rank for the card: ")
        a <- getLine
        return (parseRank a)

-- Read user's move
readMove :: String -> IO AMove
readMove msg =
    do
        putStrLn(msg)
        a <- getLine
        return (parseMove a)

-- Shuffle cards, Thank you stackoverflow community! 
shuffle :: [a] -> IO [a]
shuffle xs = do
    ar <- newArray n xs
    forM [1 .. n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
    where
        n = length xs
        newArray :: Int -> [a] -> IO (IOArray Int a)
        newArray n xs = newListArray (1,n) xs

-- Given an integer range, it will return a random number from the list
getNumberRange :: [Int] -> Int -> IO Int
getNumberRange lst index = do
    (x:xs) <- shuffle lst
    if x == index then
        getNumberRange lst index
    else
        return x

-- Parse string input to Move
parseMove :: String -> AMove
parseMove str 
    | str == "Draw" = Draw
    | str == "draw" = Draw
    | str == "Give" = Give
    | str == "give" = Give
    | str == "Go Fish" = GoFish
    | str == "go fish" = GoFish
    | str == "gofish"  = GoFish
    | str == "GoFish" = GoFish

-- Prase string input into rank
parseRank :: String -> Rank
parseRank str 
    | str == "Two" = Two
    | str == "two" = Two
    | str == "Three" = Three
    | str == "three" = Three
    | str == "Four" = Four 
    | str == "four" = Four 
    | str == "Five" = Five
    | str == "five" = Five
    | str == "Six" = Six
    | str == "six" = Six
    | str == "Seven" = Seven
    | str == "seven" = Seven
    | str == "Eight" = Eight
    | str == "eight" = Eight
    | str == "nine" = Nine
    | str == "Nine" = Nine
    | str == "ten" = Ten
    | str == "Ten" = Ten
    | str == "jack" = Jack
    | str == "Jack" = Jack
    | str == "queen" = Queen
    | str == "Queen" = Queen
    | str == "king" = King
    | str == "King" = King
    | str == "ace" = Ace
    | str == "Ace" = Ace
    | otherwise = Zero 

-- parse string input into suite
parseSuite :: String -> Suite
parseSuite str 
    | str == "Club" = Club
    | str == "club" = Club
    | str == "Diamond" = Diamond
    | str == "diamond" = Diamond
    | str == "Heart" = Heart
    | str == "heart" = Heart
    | str == "Spade" = Spade
    | str == "spade" = Spade
    | otherwise = Dummy

