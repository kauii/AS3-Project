module Utils.Utils (module Utils.Utils) where

import Types
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List(find, partition)
import Data.Char(toLower)
import qualified Data.Map as Map
import Control.Monad.State


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


-- Convert healing effect to a readable string
describeHealing :: Maybe Int -> String
describeHealing Nothing = "No healing effect."
describeHealing (Just amount) = "Heals " ++ show amount ++ " HP."

-- Convert door unlocking effect to a readable string
describeUnlock :: Maybe String -> String
describeUnlock Nothing = "No unlocking effect."
describeUnlock (Just door) = "Unlocks the door: " ++ door

-- Convert Effect to a readable string
describeEffect :: Maybe Effect -> String
describeEffect Nothing = "No effects."
describeEffect (Just effect) =
    let stats = case modifyStats effect of
                  Nothing -> []
                  Just stats -> ["Stat Changes: Max Life: " ++ show (vitality stats) ++ ", Attack: " ++ show (attack stats) ++ ", Defense: " ++ show (defense stats) ++ ", Agility: " ++ show (agility stats)]
        healing = case heal effect of
                    Nothing -> []
                    Just amount -> ["Healing: Restores " ++ show amount ++ " HP"]
        unlocking = case unlockDoor effect of
                      Nothing -> []
                      Just door -> ["Unlocks: " ++ door]
        effectDescriptions = stats ++ healing ++ unlocking
    in if null effectDescriptions
       then "No effects."
       else unlines effectDescriptions

pressEnterToContinue :: IO ()
pressEnterToContinue = do
    putStrLn "\nPress Enter to continue..."
    _ <- getLine
    return ()

displayHeader :: String -> IO ()
displayHeader caption = do
    let lineLength = length caption + 4
        border = replicate lineLength '═'
    putStrLn $ "╔" ++ border ++ "╗"
    putStrLn $ "║  " ++ caption ++ "  ║"
    putStrLn $ "╚" ++ border ++ "╝"

displaySmallHeader :: String -> IO ()
displaySmallHeader caption = do
    putStrLn $ ">>> " ++ caption ++ " <<<"

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

-- | Print health bars for enemies and player
printCombatHealthBars :: [Enemy] -> Player -> IO ()
printCombatHealthBars enemies player = do
    -- Print enemies section
    displayHeader "ENEMIES"
    mapM_ printEnemyHealth enemies

    -- Print player section
    displayHeader "PLAYER"
    putStrLn $ generateHealthBar (life player) (vitality (stats player)) 20

-- | Print a single enemy's health with a newline after each
printEnemyHealth :: Enemy -> IO ()
printEnemyHealth enemy = do
    putStrLn $ "- " ++ enemyName enemy
    putStrLn $ generateHealthBar (enemyHealth enemy) (enemyMaxHealth enemy) 20
    putStrLn ""  -- Add a blank line after each enemy

-- | Generate a single health bar
generateHealthBar :: Int -> Int -> Int -> String
generateHealthBar currentHealth maxHealth barLength =
    let filledLength = (currentHealth * barLength) `div` maxHealth
        emptyLength = barLength - filledLength
    in replicate filledLength '█' ++ replicate emptyLength '░' ++ 
       " (" ++ show currentHealth ++ "/" ++ show maxHealth ++ ")"
       
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
