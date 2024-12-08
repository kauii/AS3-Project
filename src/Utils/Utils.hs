module Utils.Utils (module Utils.Utils) where

import Types
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List(find, partition)
import Data.Char(toLower)
import qualified Data.Map as Map
import Control.Monad.State
import Assets.ProgressRelevant.Items (ancientKey)
import System.Console.ANSI
import Control.Monad (when)



-- | Parse the user input into an Action
parseAction :: String -> Maybe Action
parseAction input =
    case words (map toLower input) of  -- Normalize the input to lowercase
        ["go", dir] -> fmap Go (parseDirection dir)
        ("take" : itemWords) -> Just (Take (unwords itemWords))
        ("drop" : itemWords) -> Just (Drop (unwords itemWords))
        ("inspect" : nameWords) -> Just (Inspect (unwords nameWords))
        ("talkto" : nameWords) -> Just (TalkTo (unwords nameWords))
        ("opendoor" : nameWords) -> Just (OpenDoor (unwords nameWords))
        ("useitem" : nameWords) -> Just (UseItem (unwords nameWords))
        ["openinv"] -> Just OpenInv
        ["quit"] -> Just Quit
        _ -> Nothing

-- | Parse a direction string into a Direction
parseDirection :: String -> Maybe Direction
parseDirection dir = case map toLower dir of  -- Normalize direction to lowercase
    "north" -> Just North
    "south" -> Just South
    "east"  -> Just East
    "west"  -> Just West
    "up"    -> Just Up
    "down"  -> Just Down
    _       -> Nothing


-- | Find a room by name
findRoom :: String -> [Room] -> Room
findRoom name rooms = fromMaybe (error "Room not found!") (find (\r -> roomName r == name) rooms)

-- | Get the player's current room
getPlayerRoom :: Player -> [Room] -> Room
getPlayerRoom player = findRoom (location player)


-- | Parse the user input into an Action for Inventory Interaction
parseActionInventory :: String -> Maybe Action
parseActionInventory input =
    case words (map toLower input) of  -- Normalize the input to lowercase
        ("drop" : itemWords) -> Just (Drop (unwords itemWords))
        ("inspect" : nameWords) -> Just (Inspect (unwords nameWords))
        ("use" : nameWords) -> Just (UseItem (unwords nameWords))
        ("equip" : nameWords) -> Just (Equip (unwords nameWords))
        ["back"] -> Just Back
        _ -> Nothing

-- | Parse the user input into an Action for Fight Interaction
parseActionFight :: String -> Maybe Action
parseActionFight input =
    case words (map toLower input) of  -- Normalize the input to lowercase
        ["attack"] -> Just Attack  -- Assuming "enemy" will be replaced by a specific target
        ["openinv"] -> Just OpenInv
        ["flee"] -> Just Flee
        _ -> Nothing

-- Function to check if a flag condition is met
checkFlag :: String -> Bool -> Map.Map String Bool -> Bool
checkFlag key requiredState flags =
    case Map.lookup key flags of
        Just state -> state == requiredState
        Nothing    -> not requiredState  -- Default to False if flag not found

-- Set a flag in the GameState
setGameFlag :: String -> Bool -> StateT GameState IO ()
setGameFlag flagName flagValue = do
    state <- get
    let updatedFlags = Map.insert flagName flagValue (flags state)
    put state { flags = updatedFlags }

-- | Set the "turnEnded" flag
setTurnEnded :: Bool -> StateT GameState IO ()
setTurnEnded value = do
    state <- get
    let updatedFlags = Map.insert "turnEnded" value (flags state)
    put state { flags = updatedFlags }

-- | Check if the "turnEnded" flag is set
isTurnEnded :: StateT GameState IO Bool
isTurnEnded = do
    state <- get
    return $ Map.findWithDefault False "turnEnded" (flags state)
       
formatItem :: Item -> String
formatItem item
    | quantity item > 1 = show (quantity item) ++ "x " ++ itemName item
    | otherwise = itemName item

addItemToPlayer :: Item -> StateT GameState IO ()
addItemToPlayer newItem = do
    state <- get
    let player = playerState state
    let (matchingItems, otherItems) = partition (\i -> itemName i == itemName newItem) (inventory player)
    let updatedInventory = case matchingItems of
            [existingItem] ->
                let combinedItem = existingItem { quantity = quantity existingItem + quantity newItem }
                in combinedItem : otherItems
            [] -> newItem : otherItems
            _ -> error "Unexpected multiple items with the same name in the inventory!"
    put state { playerState = player { inventory = updatedInventory } }

removeItemFromPlayer :: String -> Int -> StateT GameState IO ()
removeItemFromPlayer itemNameToRemove qtyToRemove = do
    state <- get
    let player = playerState state
    let updatedInventory = mapMaybe (\i ->
            if itemName i == itemNameToRemove
            then
                if quantity i > qtyToRemove
                then Just i { quantity = quantity i - qtyToRemove }
                else if quantity i == qtyToRemove
                then Nothing -- Remove the item completely
                else Just i -- Do nothing if insufficient quantity
            else Just i) (inventory player)
    put state { playerState = player { inventory = updatedInventory } }

checkIfInInventory :: String -> Int -> StateT GameState IO Bool
checkIfInInventory itemNameToCheck qtyToCheck = do
    state <- get
    let player = playerState state
    return $ any (\i -> itemName i == itemNameToCheck && quantity i >= qtyToCheck) (inventory player)

addDirectionToRoom :: String -> Direction -> String -> [Room] -> [Room]
addDirectionToRoom targetRoomName direction targetRoomNameToAdd rooms =
    map (\room ->
        if roomName room == targetRoomName
        then room { exits = (direction, targetRoomNameToAdd) : exits room }
        else room) rooms


-- | Utility function to flip a switch and check the order
flipSwitch :: String -> StateT GameState IO ()
flipSwitch switchType = do
    state <- get
    let switchOrder = getSwitchOrder state
    let newOrder = switchOrder ++ [switchType]
    put $ setSwitchOrder state newOrder

    liftIO $ putStrLn $ "You flipped the " ++ switchType ++ " switch."

    -- Automatically trigger the passphrase prompt if the correct order is achieved
    when (newOrder == ["Fire", "Water", "Earth"]) $ do
        liftIO $ do
            setSGR [SetColor Foreground Vivid Green] 
            putStrLn "The switches are aligned correctly."
            setSGR [Reset] 
        speakPassphrase



-- | Set the switch order in the game state
setSwitchOrder :: GameState -> [String] -> GameState
setSwitchOrder state order =
    let updatedStringFlags = Map.insert "switch_order" order (stringFlags state)
    in state { stringFlags = updatedStringFlags }

-- | Function to check and handle the passphrase input.
speakPassphrase :: StateT GameState IO ()
speakPassphrase = do
    state <- get
    let switchOrder = getSwitchOrder state

    if switchOrder == ["Fire", "Water", "Earth"]
        then do
            liftIO $ putStrLn "Whisper the passphrase: "
            passphrase <- liftIO getLine
            if map toLower passphrase == "eternal rest"
                then do
                    addItemToPlayer ancientKey
                    liftIO $ do
                        setSGR [SetColor Foreground Vivid Magenta]
                        putStrLn "You open the cabinet and find a silver key in it. You take it."
                        setSGR [Reset]
                else liftIO $ putStrLn "The passphrase echoes into silence. Nothing happens."
        else liftIO $ putStrLn "The switches are not aligned correctly. Nothing happens."

-- | Get the current switch order from the game state.
getSwitchOrder :: GameState -> [String]
getSwitchOrder state = fromMaybe [] (Map.lookup "switch_order" (stringFlags state))

-- | Update the switch order in the game state.
updateSwitchOrder :: [String] -> StateT GameState IO ()
updateSwitchOrder order = do
    state <- get
    let updatedStringFlags = Map.insert "switch_order" order (stringFlags state)
    put state { stringFlags = updatedStringFlags }

-- | Reset the switch order to an empty list.
resetSwitchOrder :: StateT GameState IO ()
resetSwitchOrder = updateSwitchOrder []
-- | Formats PlayerStats for display, showing only non-zero values with abbreviations.
formatStats :: PlayerStats -> String
formatStats stats =
    let statList = filter (\(_, val) -> val /= 0) [
            ("HP", vitality stats),
            ("ATK", attack stats),
            ("DEF", defense stats),
            ("SPD", agility stats)  -- Using "SPD" for speed instead of "AGI"
            ]
    in if null statList
       then ""
       else "(" ++ unwords (map formatStat statList) ++ ")"
  where
    formatStat (abbr, val) = abbr ++ ": " ++ (if val > 0 then "+" else "") ++ show val

-- | Formats the stats from an item's effect if they exist.
formatStatsFromEffect :: Maybe Effect -> String
formatStatsFromEffect (Just eff) = case modifyStats eff of
    Just stats -> formatStats stats
    Nothing    -> ""
formatStatsFromEffect Nothing = ""