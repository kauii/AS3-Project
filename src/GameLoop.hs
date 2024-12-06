module GameLoop (runGameLoop, parseAction) where

import Types
import Control.Monad
import Inventory (openInventory)
import Utils (parseAction, parseDirection, getPlayerRoom, findRoom)
import Control.Monad.State
import InitialState
import Data.Maybe (fromMaybe)
import Data.List (find, intercalate)
import Data.Char (toLower)
import qualified Data.Map as Map
import RoomObjectInteraction (findObjectByName, inspectObject)
import Utils (checkFlag)

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
        putStrLn "Available actions: (Go, Take, OpenInv, Inspect, Attack, TalkTo, Quit)"

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


-- | Handle an action by modifying the game state
handleAction :: Action -> StateT GameState IO ()
handleAction action = case action of
    Go dir           -> movePlayer dir
    Take itemName    -> takeItem itemName
    Attack enemyName -> attackEnemy enemyName
    TalkTo npcName   -> talkTo npcName
    Inspect object -> inspect object
    OpenDoor doorName -> openDoor doorName
    OpenInv          -> openInventory
    Quit             -> liftIO $ putStrLn "Goodbye!"

-- | Move the player in a given direction
movePlayer :: Direction -> StateT GameState IO ()
movePlayer dir = do
    gameState <- get
    let player = playerState gameState
    let currentRoom = getPlayerRoom player (world gameState)
    let maybeExit = lookup dir (exits currentRoom)
    
    case maybeExit of
        Just exitRoomName -> do
            let maybeDoor = find (\d -> leadsTo d == exitRoomName) (doors currentRoom)
            case maybeDoor of
                Just door -> do
                    if isLocked door
                        then case keyRequired door of
                            Just keyName -> 
                                if any (\item -> itemName item == keyName) (inventory player)
                                then do
                                    -- Unlock the door and remove the key
                                    let newInventory = filter (\item -> itemName item /= keyName) (inventory player)
                                    let updatedPlayer = player { inventory = newInventory }
                                    let updatedDoor = door { isLocked = False }
                                    let updatedDoors = map (\d -> if doorName d == doorName door then updatedDoor else d) (doors currentRoom)
                                    let updatedRoom = currentRoom { doors = updatedDoors }
                                    let updatedWorld = map (\room -> if roomName room == roomName currentRoom then updatedRoom else room) (world gameState)
                                    put gameState { playerState = updatedPlayer, world = updatedWorld }
                                    liftIO $ putStrLn $ "You unlocked the door with " ++ keyName ++ "."
                                    movePlayer dir -- Retry moving after unlocking
                                else
                                    liftIO $ putStrLn "This path is currently blocked."
                            Nothing -> liftIO $ putStrLn "This path is currently blocked."
                        else do
                            let newRoom = findRoom exitRoomName (world gameState)
                            put $ gameState { playerState = player { location = exitRoomName } }
                            liftIO $ putStrLn $ "You move to " ++ exitRoomName
                Nothing -> liftIO $ putStrLn "You can't go that way!"
        Nothing -> liftIO $ putStrLn "You can't go that way!"


-- | Take item given item name
takeItem :: String -> StateT GameState IO ()
takeItem itemNameInput = do
    state <- get
    let player = playerState state
    let currentRoom = getPlayerRoom player (world state)

    let maybeItem = find (\item -> map toLower (itemName item) == map toLower itemNameInput) (items currentRoom)

    case maybeItem of
        Just item -> do
            let updatedInventory = item : inventory player
            let updatedPlayer = player { inventory = updatedInventory }
            
            let updatedRoomItems = filter (\i -> itemName i /= itemName item) (items currentRoom)
            let updatedRoom = currentRoom { items = updatedRoomItems }
            let updatedWorld = map (\room -> if roomName room == roomName currentRoom then updatedRoom else room) (world state)

            put state { playerState = updatedPlayer, world = updatedWorld }
            liftIO $ putStrLn $ "Added " ++ itemName item ++ " to your inventory."
        Nothing -> liftIO $ putStrLn $ "The item \"" ++ itemNameInput ++ "\" is not in this room."

-- | Stub functions for other actions
attackEnemy, talkTo, openDoor, useItem ::
    String -> StateT GameState IO ()

attackEnemy _ = liftIO $ putStrLn "Attack action not implemented yet."
talkTo _ = liftIO $ putStrLn "Talk action not implemented yet."
openDoor _ = liftIO $ putStrLn "OpenDoor action not implemented yet."
useItem _ = liftIO $ putStrLn "UseItem action not implemented yet."

-- Function to handle the "inspect" command
inspect :: String -> StateT GameState IO ()
inspect "room" = do
    gameState <- get
    let player = playerState gameState
    let room = getPlayerRoom player (world gameState) -- Assuming a function or field to get current room
    liftIO $ putStrLn $ description room -- Print room description
inspect objectName = do
    gameState <- get
    let player = playerState gameState
    let room = getPlayerRoom player (world gameState) -- Assuming a function or field to get current room
        maybeObject = findObjectByName objectName (roomObjects room)
    case maybeObject of
        Nothing -> liftIO $ putStrLn "Object not found."
        Just obj -> inspectObject obj


-- Print a description if its flag condition is met
printDescription :: Map.Map String Bool -> (String, String, Bool) -> IO ()
printDescription flags (desc, flagKey, requiredState) =
    when (checkFlag flagKey requiredState flags) $
        putStrLn desc
