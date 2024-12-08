module GameLoop (runGameLoop, parseAction) where

import Types
import Control.Monad
import Inventory (openInventory)
import Fight (enterCombat)
import Utils
import Control.Monad.State
import Data.List (find, intercalate)
import Data.Char (toLower)
import qualified Data.Map as Map
import RoomObjectInteraction (findObjectByName, inspectObject)
import Printer

-- | Run the game loop using StateT to manage the game state
runGameLoop :: GameState -> IO ()
runGameLoop = evalStateT gameLoop

-- | The main game loop, running within StateT monad
gameLoop :: StateT GameState IO ()
gameLoop = do
    liftIO $ putStrLn "Available actions: (Go, Take, OpenInv, Inspect, Attack, TalkTo, Quit)"
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
    Take item -> takeItem item
    TalkTo npcName   -> talkTo npcName
    Inspect object -> inspect object
    OpenInv -> do
        updatedPlayer <- openInventory False  -- Get the updated player state
        modify (\updatedState -> updatedState { playerState = updatedPlayer })  -- Update the game state
    _             -> liftIO $ putStrLn "Goodbye!"

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
                            unless (null (enemies newRoom)) $ do
                                liftIO $ putStrLn "You sense danger as you enter the room..."
                                enterCombat  -- Call the combat system
                            printRoomDescription
                Nothing -> liftIO $ putStrLn "You can't go that way!"
        Nothing -> liftIO $ putStrLn "You can't go that way!"


-- | Take item given item name
takeItem :: String -> StateT GameState IO ()
takeItem itemNameInput = do
    gameState <- get
    let player = playerState gameState
    let currentRoom = getPlayerRoom player (world gameState)

    let maybeItem = find (\item -> map toLower (itemName item) == map toLower itemNameInput) (items currentRoom)

    case maybeItem of
        Just item -> do
            let updatedInventory = item : inventory player
            let updatedPlayer = player { inventory = updatedInventory }
            
            let updatedRoomItems = filter (\i -> itemName i /= itemName item) (items currentRoom)
            let updatedRoom = currentRoom { items = updatedRoomItems }
            let updatedWorld = map (\room -> if roomName room == roomName currentRoom then updatedRoom else room) (world gameState)

            put gameState { playerState = updatedPlayer, world = updatedWorld }
            liftIO $ putStrLn $ "Added " ++ itemName item ++ " to your inventory."
        Nothing -> liftIO $ putStrLn $ "The item \"" ++ itemNameInput ++ "\" is not in this room."

talkTo :: String -> StateT GameState IO ()
talkTo npcNameInput = do
    state <- get
    let currentRoom = getPlayerRoom (playerState state) (world state)
    let maybeNPC = find (\npc -> map toLower (npcName npc) == map toLower npcNameInput) (npcs currentRoom)
    case maybeNPC of
        Just npc -> do
            case requiredItem npc of
                Just cItemName -> do
                    let player = playerState state
                    let maybeItem = find (\item -> itemName item == cItemName) (inventory player)
                    case maybeItem of
                        Just _ -> do
                            -- Remove item and execute interaction
                            let updatedInventory = filter (\item -> itemName item /= cItemName) (inventory player)
                            put state { playerState = player { inventory = updatedInventory } }
                            onInteraction npc
                        Nothing -> liftIO $ putStrLn $ dialogUnavailable npc
                Nothing -> onInteraction npc -- No item required, execute interaction
        Nothing -> liftIO $ putStrLn "There's no one by that name here."

-- Function to handle the "inspect" command
inspect :: String -> StateT GameState IO ()
inspect "room" = printRoomDescription -- Print room description
inspect object = do
    gameState <- get
    let player = playerState gameState
    let room = getPlayerRoom player (world gameState) -- Assuming a function or field to get current room
        maybeObject = findObjectByName object (roomObjects room)
    case maybeObject of
        Nothing -> liftIO $ putStrLn "Object not found."
        Just obj -> inspectObject obj
