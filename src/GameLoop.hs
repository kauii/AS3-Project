module GameLoop (runGameLoop) where

import Types
import Control.Monad.State
import InitialState
import Data.Maybe (fromMaybe)
import Data.List (find, intercalate)
import Data.Char (toLower)

-- | Run the game loop using StateT to manage the game state
runGameLoop :: GameState -> IO ()
runGameLoop = evalStateT gameLoop

-- | The main game loop, running within StateT monad
gameLoop :: StateT GameState IO ()
gameLoop = do
    state <- get
    let player = playerState state
    let currentRoom = getPlayerRoom player (world state)

     -- Extract the item names from the room
    let itemNames = map itemName (items currentRoom)
    let itemLine = if null itemNames
                   then "There are no items in the room."
                   else "The following items are in the room: " ++ intercalate ", " itemNames

    liftIO $ do
        putStrLn $ "You are in: " ++ roomName currentRoom
        putStrLn $ description currentRoom
        putStrLn itemLine
        putStrLn "Available actions: (Go, Take, Drop, Inspect, Attack, TalkTo, OpenDoor, UseItem, Quit)"

    action <- liftIO getLine
    let parsedAction = parseAction action

    case parsedAction of
        Just Quit -> liftIO $ putStrLn "Goodbye!"
        Just act  -> do
            handleAction act
            gameLoop
        Nothing -> do
            liftIO $ putStrLn "Invalid action, try again."
            gameLoop

-- | Parse the user input into an Action
parseAction :: String -> Maybe Action
parseAction input =
    case words input of
        ["Go", dir] -> fmap Go (parseDirection dir)
        ("Take" : itemWords)   -> Just (Take (unwords itemWords))
        ["Drop", item]   -> Just (Drop item)
        ["Inspect", name] -> Just (Inspect name)
        ["Attack", name] -> Just (Attack name)
        ["TalkTo", name] -> Just (TalkTo name)
        ["OpenDoor", name] -> Just (OpenDoor name)
        ["UseItem", name] -> Just (UseItem name)
        ["Quit"]         -> Just Quit
        _                -> Nothing

-- | Parse a direction string into a Direction
parseDirection :: String -> Maybe Direction
parseDirection dir = case dir of
    "North" -> Just North
    "South" -> Just South
    "East"  -> Just East
    "West"  -> Just West
    _       -> Nothing

-- | Handle an action by modifying the game state
handleAction :: Action -> StateT GameState IO ()
handleAction action = case action of
    Go dir           -> movePlayer dir
    Take itemName    -> takeItem itemName
    Drop itemName    -> dropItem itemName
    Inspect name     -> inspect name
    Attack enemyName -> attackEnemy enemyName
    TalkTo npcName   -> talkTo npcName
    OpenDoor doorName -> openDoor doorName
    UseItem itemName -> useItem itemName
    Quit             -> liftIO $ putStrLn "Goodbye!"

-- | Move the player in a given direction
movePlayer :: Direction -> StateT GameState IO ()
movePlayer dir = do
    state <- get
    let player = playerState state
    let currentRoom = getPlayerRoom player (world state)
    let maybeExit = lookup dir (exits currentRoom)
    case maybeExit of
        Just roomName -> do
            let newRoom = findRoom roomName (world state)
            put $ state { playerState = player { location = roomName } }
            liftIO $ putStrLn $ "You move to " ++ roomName
        Nothing -> liftIO $ putStrLn "You can't go that way!"

-- | Take item given item name
takeItem :: String -> StateT GameState IO ()
takeItem itemNameInput = do
    state <- get
    let player = playerState state
    let currentRoom = getPlayerRoom player (world state)

    -- Find the item in the current room
    let maybeItem = find (\item -> map toLower (itemName item) == map toLower itemNameInput) (items currentRoom)

    case maybeItem of
        Just item -> do
            -- Add the item to the player's inventory
            let updatedInventory = item : inventory player
            let updatedPlayer = player { inventory = updatedInventory }
            
            -- Remove the item from the room
            let updatedRoomItems = filter (\i -> itemName i /= itemName item) (items currentRoom)
            let updatedRoom = currentRoom { items = updatedRoomItems }
            let updatedWorld = map (\room -> if roomName room == roomName currentRoom then updatedRoom else room) (world state)

            -- Update the game state
            put state { playerState = updatedPlayer, world = updatedWorld }
            liftIO $ putStrLn $ "You picked up " ++ itemName item ++ "."
        Nothing -> liftIO $ putStrLn $ "The item \"" ++ itemNameInput ++ "\" is not in this room."

-- | Stub functions for other actions
dropItem, inspect, attackEnemy, talkTo, openDoor, useItem ::
    String -> StateT GameState IO ()
dropItem _ = liftIO $ putStrLn "Drop action not implemented yet."
inspect _ = liftIO $ putStrLn "Inspect action not implemented yet."
attackEnemy _ = liftIO $ putStrLn "Attack action not implemented yet."
talkTo _ = liftIO $ putStrLn "Talk action not implemented yet."
openDoor _ = liftIO $ putStrLn "OpenDoor action not implemented yet."
useItem _ = liftIO $ putStrLn "UseItem action not implemented yet."

-- | Find a room by name
findRoom :: String -> [Room] -> Room
findRoom name rooms = fromMaybe (error "Room not found!") (find (\r -> roomName r == name) rooms)

-- | Get the player's current room
getPlayerRoom :: Player -> [Room] -> Room
getPlayerRoom player = findRoom (location player)