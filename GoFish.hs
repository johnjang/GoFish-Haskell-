-- CPSC 312 PROJECT 2 GO FISH --
-- Authors: Nafisa Shazia   18769142   
--          John Jang       52043122  
--
-- For our project, we are implementing a game called Go Fish.
-- (about the game: https://cardgames.io/gofish/#about)
--
-- This class is responsible for the actual game states

-- to run, in GHCI
--
-- :l Play.hs
-- goFishStart


module GoFish where


import Data.Function (on)   -- For sorting
import Data.List            -- For sorting/list management

-- Rank of the cards, Zero is there for error/placeholder
data Rank = Zero|Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King|Ace
    deriving (Enum, Show, Eq, Ord)
-- Suite of the cards, Dummy is there for error/placeholder
data Suite = Dummy|Club|Diamond|Heart|Spade
    deriving (Enum, Show, Eq, Ord)

type Card = (Rank, Suite)   -- A tuple representing a card. Int represents a
                            --  number and String one of the four suits.
                            --  Jack, Queen, King, Ace will be 11,12,13,14 respectively.


type Deck = [Card]  -- A deck of card is an array of cards
        

data Player = Player { pid::Int  -- An unique ID of the player
                     , hands::Deck  -- Deck of cards the player is currently holding onto
                     , collected::[Rank]} deriving (Eq) -- Deck of collected cards
instance Show Player where 
    show (Player a b c) = "\nPlayer ID: " ++ show a ++ "\nHands: " ++ show b ++ 
                        "\nCollected deck: " ++ show c

-- List of moves possible
data AMove = Ask Int Card       -- Ask a specified player for a card
           | Give               -- Give a specified player a card
           | GoFish             -- Gofish to the specified player
           | Draw               -- Just simply draw a card
                                -- Total of four moves are available in the game
    deriving (Eq)
instance Show AMove where
   show (Ask a b) = "\nAsking player " ++ show a ++ " for card " ++ show b ++ "\n"
   show (Give) = "\nGiving a card!\n"
   show (GoFish) = "\nGo Fish!\n"
   show (Draw) = "\nDrawing a card from the deck!\n"


-- Information of each state
data Info = Info { middleDeck :: Deck         -- Game deck
                 , players :: [Player]        -- Players who are playing the game
                 , source :: Int              -- The source of the player (respective player's turn)
                 , destination :: Int         -- The destination of action (if exists)
                 } 
instance Show Info where
    show (Info deck players source destination) =
        "\nPlayer " ++ show source ++ "'s info:" ++ 
        show (players!!source) ++ "\n"

-- Game states
data State = StartS Info         -- Initial stage for each player 
           | GoFishS Card Info   -- Saying GoFish state
           | GiveS Card Info     -- Giving a card state
           | DrawS Info          -- Drawing a card state
           | EndS Int 
instance Show State where
    show (StartS info) = "\nPlayer " ++ show (source info) ++ "'s turn to play.\n\n" ++
                            "Choose a player number and a card." ++ show info ++ "\n" 
                            ++ "Currently " ++ show ( length $ middleDeck info ) ++
                                " cards are remaining in the deck.\n\n"

    show (GoFishS card info) = "\nPlayer " ++ show (source info) ++ "'s turn to play.\n\n" ++ 
                               "Player " ++ show (destination info) ++ " is asking for card: " ++
                               show (fst card) ++ ", " ++ "Say go fish or give.\n"++ show info
                            ++ "Currently " ++ show ( length $ middleDeck info ) ++
                                " cards are remaining in the deck.\n\n"

    show (GiveS card info) = "\nPlayer " ++ show (source info) ++ "'s turn to play.\n\n" ++ 
                               "Player " ++ show (destination info) ++ " is asking for card: " ++
                               show (fst card) ++ ", " ++ "Say go fish or give.\n"++ show info
                            ++ "Currently " ++ show ( length $ middleDeck info ) ++
                                " cards are remaining in the deck.\n\n"

    show (DrawS info) = "\nPlayer " ++ show (source info) ++ "'s turn to play.\n\n" ++
                         "Player can only draw a card at the moment. Type Draw to draw a card.\n"
                            ++ "Currently " ++ show ( length $ middleDeck info ) ++
                                " cards are remaining in the deck.\n\n"

    show (EndS winner) = "\nEnd of the game, the winner is " ++ show winner ++ ".\n"


type Action = (State, AMove)    -- Action is a single move with a state
type Result = (State, Bool)     -- Result is a state and a bool to indicate the validness
                                --  of the state

----------------------------------------

goFish :: Action -> Result      -- A game takes in an action and returns a result

-- Initial state of the game/initial state of each respective player's turn
goFish (StartS info, Ask player card) =
    if checkCard card $ (players info)!!player then 
        (GiveS card $ Info (middleDeck info) (players info) player (source info), True)
    else 
        (GoFishS card $ Info (middleDeck info) (players info) player (source info), True)

-- State where player says GoFish
goFish (GoFishS card info, GoFish) = 
    (DrawS $ Info (middleDeck info) (players info) (destination info) (destination info), True)

-- State where player gives the cards to the requested player
goFish (GiveS card info, Give) = let
    giver = (players info) !! (source info)
    taker = (players info) !! (destination info) 
    removedDeck = getRemoved card (hands ((players info)!!(source info))) in
    (StartS $ Info  
   (middleDeck info) 
    (replacePlayer (takeCard giver card) (replacePlayer (giveCard taker removedDeck) (players info))) 
    (destination info) (destination info), True )

-- State where player draws a card from the deck
goFish (DrawS info, Draw) = let
    (x:xs) = middleDeck info 
    source' = (players info) !! (source info) 
    nextPlayer = getNextPlayer (source info) (players info) in
    if length (x:xs) == 1 then
        let players' = replacePlayer (giveCard source' [x]) (players info) in
        (EndS $ checkWinner players', True)
    else 
        if (null $ hands ((players info)!!nextPlayer)) then  --go draw
            (DrawS $ Info xs (replacePlayer (giveCard source' [x]) (players info))
            (nextPlayer) (source info), True)
        else
            (StartS $ Info xs (replacePlayer (giveCard source' [x]) (players info))
            (nextPlayer) (source info), True)


------ Helper functions ------

-- Return's a starting state given a random deck and number of players and number of cards to initially
--  deal to each players
start :: Int -> Int -> Deck -> State 
start numOfPlayers numOfCards deck' = let
    (players, deck) = dealPlayers numOfPlayers (makePlayers numOfPlayers) numOfCards
            (deck') in   --That is deck later on
    ((StartS $ Info deck players 0 0))

--given list of players and a deck, deal
dealPlayers :: Int -> [Player] -> Int -> Deck -> ([Player], Deck)
dealPlayers 0 players numToDeal deck = (players, deck)
dealPlayers num players numToDeal deck = let
    newPlayer = dealPlayer (players!!(num-1)) numToDeal deck in
    dealPlayers (num-1) ((take (num-1) players) ++ [newPlayer] 
                            ++ (drop (num) players) ) numToDeal (getNLast numToDeal deck)

-- Given a player, deal the player n amount of cards and return a new deck.
dealPlayer :: Player -> Int -> Deck -> Player
dealPlayer player num deck = let
    pid' = pid player in
    (Player pid' (getNFirst num deck) [])

-- Make n number of players
makePlayers :: Int -> [Player]
makePlayers num =
    [Player x [] [] | x <- [0 .. (num-1) ] ]

-- Get first n elements from a deck
getNFirst :: Int -> Deck -> Deck
getNFirst 0 list = []
getNFirst num list = [(head list)] ++ getNFirst (num-1) (tail list)

-- Get a new list without first n elements from a deck
getNLast :: Int -> Deck -> Deck
getNLast 0 list = list
getNLast num list = getNLast (num-1) (tail list)

-- Check the winner of the current state
checkWinner :: [Player] -> Int
checkWinner players = findMax [length $ collected x | x<-players]
findMax (x:xs) =
    if x == (maximum (x:xs)) then
        0   
    else
        1 + findMax xs

-- Check if player has the card
checkCard :: Card -> Player -> Bool
checkCard card player = let
    hands' = hands player in
    or [checkNum x card | x <- hands']

-- Check if two cards are equal rank 
checkNum :: Card -> Card -> Bool
checkNum card1 card2 = let
    rank1 = fst card1
    rank2 = fst card2 in
    rank1 == rank2

-- Take a card away from a player
takeCard :: Player -> Card -> Player
takeCard player card = 
    if (checkCard card player) then
        let hands' = hands player in
        Player (pid player) (removeCard card hands') (collected player)
    else
        player

-- Give a card to the player
giveCard :: Player -> Deck -> Player -- needs to give ALL cards
giveCard player [] = player
giveCard player deck = let
    hands' = sort $ deck ++ (hands player)
    collected' = collected player in
    if checkIfFour hands' /= Zero then
        Player (pid player) 
        (removeCard ((checkIfFour hands'),Dummy) hands') 
        ( sort $ [(fst $ head deck)] ++ collected')
    else
        Player (pid player) hands' collected'

-- Remove all related cards from the deck
removeCard :: Card -> Deck -> Deck
removeCard _ [] = []
removeCard card (x:xs) = let
    (num1, _) = card
    (num2, _) = x in
    if num1 == num2 then
        removeCard card xs
    else x:removeCard card xs

-- Get removed cards from the deck
getRemoved :: Card -> Deck -> Deck
getRemoved _ [] = []
getRemoved card (x:xs) = let
    (num1, _) = card
    (num2, _) = x in
    if num1 == num2 then
        x:getRemoved card xs
    else getRemoved card xs


-- Given a card, make a frequency table (used to check if a player has 4 cards
checkIfFour :: Deck -> Rank 
checkIfFour [] = Zero
checkIfFour deck = checkIfFour' $ map (\x -> (head x, length x)) $ group $ sort [x | (x,y) <- deck]

--given a tuple, return fst if snd is 4
checkIfFour' :: [(Rank,Int)] -> Rank 
checkIfFour' [] = Zero
checkIfFour' ((a,b):x) =
    if b == 4 then
        a
    else
        checkIfFour' x

-- Get next player given a list of player and current ID
getNextPlayer :: Int -> [Player] -> Int
getNextPlayer id plyers =
    if id < (length plyers-1) then
        id+1
    else
        0

-- Replace a given player with new info from the list of players
replacePlayer :: Player -> [Player] -> [Player]
replacePlayer _ [] = []
replacePlayer player (x:xs) =
    if (pid player) == (pid x) then
        player : replacePlayer player xs
    else
        x : replacePlayer player xs

-- Sort the given deck from least to greatest
sortDeck :: Deck -> Deck
sortDeck deck = sortBy (compare `on` fst) deck

