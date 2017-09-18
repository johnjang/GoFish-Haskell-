-- CPSC 312 PROJECT 2 GO FISH --
-- Authors: Nafisa Shazia   18769142   
--          John Jang       52043122  
--
-- For our project, we are implementing a game called Go Fish.
-- This game is originally created by Einar Egilsson (admin@cardgames.io)
-- (about the original game: https://cardgames.io/gofish/#about)
--
-- We have re-implemented the game using Haskell
--

type Card = (Int, String)   -- A tuple representing a card. Int represents a
                            --  number and String one of the four suits.
                            --  Jack, Queen, King, Ace will be 11,12,13,14 respectively.
                            --
type Deck = [Card]  -- A deck of card is an array of cards


data Player = Player { pid::Int  -- An unique ID of the player
                     , hands::Deck  -- Deck of cards the player is currently holding onto
                     , collected::Deck } deriving (Show, Eq) -- Deck of collected cards

data AMove = Ask Player Card    -- Ask a specified player for a card
           | Give Player Card   -- Give a specified player a card
           | GoFish Player      -- Gofish to the specified player
           | Wait Player        -- Just simply wait. Used to move from an iterim states
                    -- Total of four moves are available in the game
           deriving (Show)

data Info = Info { middleDeck :: Deck   -- Game deck
                 , players :: [Player]  -- Players who are playing the game
                 , source :: Player     -- The source of the player (respective player's turn)
                 } deriving (Show)
                 

data State = ChooseS Info   -- Game state where the source needs to choose a player for a Card
           | AskS Info      -- Game state where the source is asking a specific player for a Card
           | GoFishS Info   -- Game state where the source is saying GoFish to the specific player
           | GiveS Info     -- Game state where the source is giving a card to the specific player
           | DrawS Info     -- Game state where the source is drawing a card from the game deck
           | EndS Info      -- End game state 
           deriving (Show)


type Action = (State, AMove)    -- Action is a single move with a state
type Result = (State, Bool)     -- Result is a state and a bool to indicate the validness
                                --  of the state

goFish :: Action -> Result      -- A game takes in an action and returns a result
goFish (ChooseS info, Ask plyer) =
    (AskS $ Info (middleDeck info) (players info) plyer, True)

goFish (AskS info, Give player card) = 
    --if player has the card go to GoFish state, else Give state
    --go through source player 



--goFish (AskS info, GoFish player) = 
    



--goFish (GoFishS info, Wait player) =
goFish (GoFishS info, Wait plyer) =
    (DrawS $ Info (middleDeck info) (players info) plyer, True)

-- CPSC 312 PROJECT 2 GO FISH --
-- Authors: Nafisa Shazia   18769142   
--          John Jang       52043122  
--
-- For our project, we are implementing a game called Go Fish.
-- This game is originally created by Einar Egilsson (admin@cardgames.io)
-- (about the original game: https://cardgames.io/gofish/#about)
--
-- We have re-implemented the game using Haskell
--

type Card = (Int, String)   -- A tuple representing a card. Int represents a
                            --  number and String one of the four suits.
                            --  Jack, Queen, King, Ace will be 11,12,13,14 respectively.
                            --
type Deck = [Card]  -- A deck of card is an array of cards


data Player = Player { pid::Int  -- An unique ID of the player
                     , hands::Deck  -- Deck of cards the player is currently holding onto
                     , collected::Deck } deriving (Show, Eq) -- Deck of collected cards

data AMove = Ask Player Card    -- Ask a specified player for a card
           | Give Player Card   -- Give a specified player a card
           | GoFish Player      -- Gofish to the specified player
           | Wait Player        -- Just simply wait. Used to move from an iterim states
                    -- Total of four moves are available in the game
           deriving (Show)
-- CPSC 312 PROJECT 2 GO FISH --
-- Authors: Nafisa Shazia   18769142   
--          John Jang       52043122  
--
-- For our project, we are implementing a game called Go Fish.
-- This game is originally created by Einar Egilsson (admin@cardgames.io)
-- (about the original game: https://cardgames.io/gofish/#about)
--
-- We have re-implemented the game using Haskell
--

type Card = (Int, String)   -- A tuple representing a card. Int represents a
                            --  number and String one of the four suits.
                            --  Jack, Queen, King, Ace will be 11,12,13,14 respectively.
                            --
type Deck = [Card]  -- A deck of card is an array of cards


data Player = Player { pid::Int  -- An unique ID of the player
                     , hands::Deck  -- Deck of cards the player is currently holding onto
                     , collected::Deck } deriving (Show, Eq) -- Deck of collected cards

data AMove = Ask Player Card    -- Ask a specified player for a card
           | Give Player Card   -- Give a specified player a card
           | GoFish Player      -- Gofish to the specified player
           | Wait Player        -- Just simply wait. Used to move from an iterim states
                    -- Total of four moves are available in the game
           deriving (Show)

data Info = Info { middleDeck :: Deck   -- Game deck
                 , players :: [Player]  -- Players who are playing the game
                 , source :: Player     -- The source of the player (respective player's turn)
                 } deriving (Show)
                 

data State = ChooseS Info   -- Game state where the source needs to choose a player for a Card
           | AskS Info      -- Game state where the source is asking a specific player for a Card
           | GoFishS Info   -- Game state where the source is saying GoFish to the specific player
           | GiveS Info     -- Game state where the source is giving a card to the specific player
           | DrawS Info     -- Game state where the source is drawing a card from the game deck
           | EndS Info      -- End game state 
           deriving (Show)


type Action = (State, AMove)    -- Action is a single move with a state
type Result = (State, Bool)     -- Result is a state and a bool to indicate the validness
                                --  of the state

goFish :: Action -> Result      -- A game takes in an action and returns a result
goFish (ChooseS info, Ask plyer) =
    (AskS $ Info (middleDeck info) (players info) plyer, True)

goFish (AskS info, Give player card) = 
    --if player has the card go to GoFish state, else Give state
    --go through source player 



--goFish (AskS info, GoFish player) = 
    



--goFish (GoFishS info, Wait player) =
goFish (GoFishS info, Wait plyer) =
    (DrawS $ Info (middleDeck info) (players info) plyer, True)

--Draw a card
goFish (DrawS info, Wait nPlyer) =
    let card        = head $ middleDeck info
        (carH:carT) = drop 1 $ middleDeck info
        plyers      = players info
        src         = source info in
        if null (carH:carT) then    --game over
            (EndS $ Info (carH:carT) [plyAddCrd x src card|x<-plyers] 
            nPlyer, True)
        else
            (ChooseS $ Info (carH:carT) [plyAddCrd x src card|x<-plyers] 
            nPlyer, True)
             
--Catching an invalid move
goFish (previousState, _) = (previousState, False)

-----Helper functions--------

--Add a card to the player's deck if given id matches
plyAddCrd :: Player -> Player -> Card -> Player
plyAddCrd player player' card = 
    if player == player'
        then
            let hands'     = hands player 
                pid'       = pid player
                collected' = collected player in
                Player pid' (card:hands') collected'
    else
        player

--Given a list of Players and an index, return the next player
nextPlayer :: [Player] -> Player -> Player
nextPlayer (x:xs) a = if last (x:xs) == a
                        then x
                      else if x == a
                        then head xs
                      else
                        nextPlayer xs a




--Check if game is over  --not used
--gameOverCondition :: State -> Bool
gameOverCondition _ info =
    if null $ middleDeck info 
        then True
        else False






data Info = Info { middleDeck :: Deck   -- Game deck
                 , players :: [Player]  -- Players who are playing the game
                 , source :: Player     -- The source of the player (respective player's turn)
                 } deriving (Show)
                 

data State = ChooseS Info   -- Game state where the source needs to choose a player for a Card
           | AskS Info      -- Game state where the source is asking a specific player for a Card
           | GoFishS Info   -- Game state where the source is saying GoFish to the specific player
           | GiveS Info     -- Game state where the source is giving a card to the specific player
           | DrawS Info     -- Game state where the source is drawing a card from the game deck
           | EndS Info      -- End game state 
           deriving (Show)


type Action = (State, AMove)    -- Action is a single move with a state
type Result = (State, Bool)     -- Result is a state and a bool to indicate the validness
                                --  of the state

goFish :: Action -> Result      -- A game takes in an action and returns a result
goFish (ChooseS info, Ask plyer) =
    (AskS $ Info (middleDeck info) (players info) plyer, True)

goFish (AskS info, Give player card) = 
    --if player has the card go to GoFish state, else Give state
    --go through source player 



--goFish (AskS info, GoFish player) = 
    



--goFish (GoFishS info, Wait player) =
goFish (GoFishS info, Wait plyer) =
    (DrawS $ Info (middleDeck info) (players info) plyer, True)

--Draw a card
goFish (DrawS info, Wait nPlyer) =
    let card        = head $ middleDeck info
        (carH:carT) = drop 1 $ middleDeck info
        plyers      = players info
        src         = source info in
        if null (carH:carT) then    --game over
            (EndS $ Info (carH:carT) [plyAddCrd x src card|x<-plyers] 
            nPlyer, True)
        else
            (ChooseS $ Info (carH:carT) [plyAddCrd x src card|x<-plyers] 
            nPlyer, True)
             
--Catching an invalid move
goFish (previousState, _) = (previousState, False)

-----Helper functions--------

--Add a card to the player's deck if given id matches
plyAddCrd :: Player -> Player -> Card -> Player
plyAddCrd player player' card = 
    if player == player'
        then
            let hands'     = hands player 
                pid'       = pid player
                collected' = collected player in
                Player pid' (card:hands') collected'
    else
        player

--Given a list of Players and an index, return the next player
nextPlayer :: [Player] -> Player -> Player
nextPlayer (x:xs) a = if last (x:xs) == a
                        then x
                      else if x == a
                        then head xs
                      else
                        nextPlayer xs a




--Check if game is over  --not used
--gameOverCondition :: State -> Bool
gameOverCondition _ info =
    if null $ middleDeck info 
        then True
        else False





--Draw a card
goFish (DrawS info, Wait nPlyer) =
    let card        = head $ middleDeck info
        (carH:carT) = drop 1 $ middleDeck info
        plyers      = players info
        src         = source info in
        if null (carH:carT) then    --game over
            (EndS $ Info (carH:carT) [plyAddCrd x src card|x<-plyers] 
            nPlyer, True)
        else
            (ChooseS $ Info (carH:carT) [plyAddCrd x src card|x<-plyers] 
            nPlyer, True)
             
--Catching an invalid move
goFish (previousState, _) = (previousState, False)

-----Helper functions--------

--Add a card to the player's deck if given id matches
plyAddCrd :: Player -> Player -> Card -> Player
plyAddCrd player player' card = 
    if player == player'
        then
            let hands'     = hands player 
                pid'       = pid player
                collected' = collected player in
                Player pid' (card:hands') collected'
    else
        player

--Given a list of Players and an index, return the next player
nextPlayer :: [Player] -> Player -> Player
nextPlayer (x:xs) a = if last (x:xs) == a
                        then x
                      else if x == a
                        then head xs
                      else
                        nextPlayer xs a




--Check if game is over  --not used
--gameOverCondition :: State -> Bool
gameOverCondition _ info =
    if null $ middleDeck info 
        then True
        else False





