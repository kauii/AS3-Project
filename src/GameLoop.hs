module GameLoop (runGameLoop, parseAction) where

import Types
import Control.Monad
import Inventory (openInventory)
import Fight (enterCombat)
import Utils.Utils
import Control.Monad.State
import Data.List (find, partition)
import Data.Char (toLower)
import RoomObjectInteraction (findObjectByName, inspectObject)
import Utils.Printer
import System.Console.ANSI
import System.Exit
import Assets.ProgressRelevant.Boss (enterBossBattle)

-- | Run the game loop using StateT to manage the game state
runGameLoop :: GameState -> IO ()
runGameLoop = evalStateT gameLoop

-- | The main game loop, running within StateT monad
gameLoop :: StateT GameState IO ()
gameLoop = do
    gameState <- get
    let player = playerState gameState
    let health = life player

    when (health <= 0) $ do
        liftIO $ printColored Red "You have died. Game Over!"
        liftIO exitSuccess -- Exit the loop

    liftIO $ printAvailableActions ["Go", "Take", "OpenInv", "Inspect", "TalkTo", "Quit"]
    action <- liftIO getLine
    let parsedAction = parseAction action

    case parsedAction of
        Just Quit -> liftIO $ printColored Red "Goodbye!"
        Just act  -> do
            handleAction act
            gameLoop
        Nothing -> do
            liftIO $ printColored Yellow "Invalid action, try again."
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
    _ -> liftIO $ printColored Yellow "You can't do this here!"

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
                                    let updatedDoor = door { isLocked = False }
                                    let updatedDoors = map (\d -> if doorName d == doorName door then updatedDoor else d) (doors currentRoom)
                                    let updatedRoom = currentRoom { doors = updatedDoors }
                                    let updatedWorld = map (\room -> if roomName room == roomName currentRoom then updatedRoom else room) (world gameState)
                                    put gameState { world = updatedWorld }
                                    liftIO $ printColored Green $ "You unlocked the door with " ++ keyName ++ "."
                                    movePlayer dir -- Retry moving after unlocking
                                else
                                    liftIO $ printColored Yellow $ "This path is currently blocked. You need " ++ keyName ++ " to progress."
                            Nothing -> liftIO $ printColored Red "This path is currently blocked."
                        else do
                            let newRoom = findRoom exitRoomName (world gameState)
                            put $ gameState { playerState = player { location = exitRoomName } }
                            liftIO $ printColored Green $ "You move to " ++ exitRoomName
                            case difficulty newRoom of
                                Boss -> do
                                    liftIO $ printColored Red "You sense an overwhelming presence... The final battle awaits!"
                                    enterBossBattle  -- Trigger the boss battle
                                _ -> unless (null (enemies newRoom)) $ do
                                    liftIO $ printColored Red "You sense danger as you enter the room..."
                                    enterCombat  -- Call the regular combat system
                            printRoomDescription
                Nothing -> do
                    -- Move to the room if no door blocks the path
                    let newRoom = findRoom exitRoomName (world gameState)
                    put $ gameState { playerState = player { location = exitRoomName } }
                    liftIO $ printColored Green $ "You move to " ++ exitRoomName
                    case difficulty newRoom of
                        Boss -> do
                            liftIO $ printColored Red "You sense an overwhelming presence... The final battle awaits!"
                            enterBossBattle  -- Trigger the boss battle
                        _ -> unless (null (enemies newRoom)) $ do
                            liftIO $ printColored Red "You sense danger as you enter the room..."
                            enterCombat  -- Call the regular combat system
                    printRoomDescription
        Nothing -> liftIO $ printColored Yellow "You can't go that way!"



-- | Take item given item name
takeItem :: String -> StateT GameState IO ()
takeItem itemNameInput = do
    gameState <- get
    let player = playerState gameState
    let currentRoom = getPlayerRoom player (world gameState)

    let maybeItem = find (\item -> map toLower (itemName item) == map toLower itemNameInput) (items currentRoom)

    case maybeItem of
        Just item -> do
             -- Check if the item already exists in the inventory
            let (existingItems, otherItems) = partition (\i -> itemName i == itemName item) (inventory player)
            let updatedInventory = case existingItems of
                    [existingItem] -> 
                        let combinedItem = existingItem { quantity = quantity existingItem + quantity item }
                        in combinedItem : otherItems
                    [] -> item : otherItems
                    _ -> otherItems

            let updatedPlayer = player { inventory = updatedInventory }
            
            let updatedRoomItems = filter (\i -> itemName i /= itemName item) (items currentRoom)
            let updatedRoom = currentRoom { items = updatedRoomItems }
            let updatedWorld = map (\room -> if roomName room == roomName currentRoom then updatedRoom else room) (world gameState)

            put gameState { playerState = updatedPlayer, world = updatedWorld }
            liftIO $ printColored Green $ "Added " ++ itemName item ++ " to your inventory."
            liftIO $ putStrLn $ "\n" ++ itemAscii item  -- Display the ASCII art
        Nothing -> liftIO $ printColored Yellow $ "The item \"" ++ itemNameInput ++ "\" is not in this room."

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
                        Just item -> do
                            liftIO $ printColored Magenta $ "You give " ++ npcName npc ++ " " ++ itemName item
                            -- Update inventory: decrement quantity or remove item
                            let updatedInventory = 
                                    if quantity item > 1
                                    then map (\i -> if itemName i == cItemName 
                                                    then i { quantity = quantity i - 1 }
                                                    else i) 
                                             (inventory player)
                                    else filter (\i -> itemName i /= cItemName) (inventory player)

                            -- Update the player state and execute interaction
                            put state { playerState = player { inventory = updatedInventory } }
                            onInteraction npc
                        Nothing -> liftIO $ putStrLn $ dialogUnavailable npc
                Nothing -> onInteraction npc -- No item required, execute interaction
        Nothing -> liftIO $ printColored Yellow "There's no one by that name here."

-- Function to handle the "inspect" command
inspect :: String -> StateT GameState IO ()
inspect "room" = printRoomDescription -- Print room description
inspect objectName = do
    gameState <- get
    let player = playerState gameState
    let room = getPlayerRoom player (world gameState) -- Assuming a function or field to get current room
        maybeObject = findObjectByName objectName (roomObjects room)
    case maybeObject of
        Nothing -> liftIO $ printColored Yellow "Object not found."
        Just obj -> inspectObject obj
