module Inventory (openInventory) where

import Types
import Utils.Utils
import Control.Monad.State
import Control.Monad
import Data.List (find, partition)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Control.Applicative ((<|>))
import System.Console.ANSI

-- | Opens the player's inventory and displays stats, gear, and items.
openInventory :: Bool -> StateT GameState IO Player
openInventory inCombat = do
    state <- get
    let player = playerState state

    liftIO $ displayHeader "INVENTORY"
    
    liftIO $ displaySmallHeader "Player Stats"
    liftIO $ putStrLn $ "  Life:    " ++ show (life player) ++ "/" ++ show (vitality (stats player))
    liftIO $ putStrLn $ "  Attack:  " ++ show (attack (stats player))
    liftIO $ putStrLn $ "  Defense: " ++ show (defense (stats player))
    liftIO $ putStrLn $ "  Agility: " ++ show (agility (stats player))
    liftIO $ putStrLn ""

    liftIO $ displaySmallHeader "Equipped Gear"
    case weapon player of
        Just w  -> liftIO $ putStrLn $ "  Weapon: " ++ colorize Yellow (itemName w) ++ " " ++ formatStatsFromEffect (effect w)
        Nothing -> liftIO $ putStrLn "  Weapon: None"
    case armor player of
        Just a  -> liftIO $ putStrLn $ "  Armor:  " ++ colorize Cyan (itemName a) ++ " " ++ formatStatsFromEffect (effect a)
        Nothing -> liftIO $ putStrLn "  Armor:  None"
    liftIO $ putStrLn ""
    
    liftIO $ displaySmallHeader "Items"
    let inventoryItems = inventory player
    liftIO $ if null inventoryItems
        then putStrLn "Your inventory is empty."
        else mapM_ (putStrLn . ("  - " ++) . colorize Magenta . itemName) inventoryItems
    inventoryInteraction inCombat player

-- | Handles player interaction within the inventory.
inventoryInteraction :: Bool -> Player -> StateT GameState IO Player
inventoryInteraction inCombat player = do
    let actions = if inCombat
                    then ["Use", "Equip", "Inspect", "Back"]
                    else ["Drop", "Use", "Equip", "Inspect", "Back"]
    liftIO $ printAvailableActions actions
    action <- liftIO getLine
    let parsedAction = parseActionInventory action

    case parsedAction of
        Just (UseItem itemNameInput) -> do
            newPlayer <- useItem itemNameInput inCombat
            if inCombat
                then return newPlayer
                else openInventory inCombat
        Just (Equip itemNameInput) -> do
            equipItem itemNameInput
            openInventory inCombat
        Just (Drop itemNameInput)
            | not inCombat -> dropItem itemNameInput >> openInventory inCombat
            | otherwise -> restrictedAction "Drop" >> openInventory inCombat
        Just (Inspect itemNameInput) -> do
            inspect itemNameInput
            openInventory inCombat
        Just Back -> return player
        _ -> do
            liftIO $ putStrLn "Invalid action, try again."
            inventoryInteraction inCombat player

-- | Restricts actions that are not allowed during combat.
restrictedAction :: String -> StateT GameState IO ()
restrictedAction action = liftIO $ putStrLn $ action ++ " is not allowed in combat!"

-- | Uses an item from the inventory, applying its effect if applicable.
useItem :: String -> Bool -> StateT GameState IO Player
useItem itemNameInput inCombat = do
    state <- get
    let player = playerState state
    let maybeItem = find (\item -> map toLower (itemName item) == map toLower itemNameInput) (inventory player)

    case maybeItem of
        Just item -> case itemType item of
            Consumable -> do
                let newPlayer = applyEffect (effect item) player
                -- Update inventory: decrement quantity or remove if quantity reaches 0
                let updatedInventory = 
                        if quantity item > 1
                                then map (\i -> if itemName i == itemName item 
                                    then i { quantity = quantity i - 1 }
                                    else i) 
                                        (inventory newPlayer)
                                else filter (\i -> itemName i /= itemName item) (inventory newPlayer)

                let finalPlayer = newPlayer { inventory = updatedInventory }
                put state { playerState = finalPlayer }
                when inCombat (setTurnEnded True)
                liftIO $ do
                    putStrLn $ "\n" ++ itemAscii item  -- Display the ASCII art
                    putStrLn $ "You used the " ++ itemName item ++ "."
                    printColored Green "Effects applied!"
                    pressEnterToContinue
                return finalPlayer
            _ -> do
                liftIO $ printColored Red "This item cannot be used."
                return player
        Nothing -> do
            liftIO $ printColored Yellow $ "The item \"" ++ itemNameInput ++ "\" is not in your inventory."
            return player

-- | Drops an item from the inventory and places it in the current room.
dropItem :: String -> StateT GameState IO ()
dropItem itemNameInput = do
    state <- get
    let player = playerState state
    let currentRoom = getPlayerRoom player (world state)
    let maybeItem = find (\item -> map toLower (itemName item) == map toLower itemNameInput) (inventory player)

    case maybeItem of
        Just item -> do
            let updatedInventory = filter (\i -> itemName i /= itemName item) (inventory player)
            let updatedPlayer = player { inventory = updatedInventory }

            -- Add the item to the current room
            -- Check if an item with the same name already exists in the room
            let (matchingRoomItems, otherRoomItems) = partition (\i -> itemName i == itemName item) (items currentRoom)
            let updatedRoomItems = case matchingRoomItems of
                    [existingItem] -> 
                        let combinedItem = existingItem { quantity = quantity existingItem + quantity item }
                        in combinedItem : otherRoomItems
                    [] -> item : otherRoomItems
                    _ -> otherRoomItems

            -- Update the room and world
            let updatedRoom = currentRoom { items = updatedRoomItems }
            let updatedWorld = map (\room -> if roomName room == roomName currentRoom then updatedRoom else room) (world state)


            -- Update the game state
            put state { playerState = updatedPlayer, world = updatedWorld }
            liftIO $ putStrLn $ "You dropped " ++ itemName item ++ "."
        Nothing -> liftIO $ printColored Yellow "Item not found."

-- | Equips an item (either a weapon or armor) from the inventory and applies its effects.
equipItem :: String -> StateT GameState IO ()
equipItem itemNameInput = do
    state <- get
    let player = playerState state
    let maybeItem = find (\item -> map toLower (itemName item) == map toLower itemNameInput) (inventory player)

    case maybeItem of
        Just item -> case itemType item of
            Sword -> do
                let updatedInventory = filter (\i -> itemName i /= itemName item) (inventory player)
                let playerWithEffect = applyEffect (effect item) player
                let updatedPlayer = playerWithEffect { weapon = Just item, inventory = updatedInventory }
                put state { playerState = updatedPlayer }
                liftIO $ printColored Green $ "You equipped the weapon: " ++ itemName item ++ "."
            Armor -> do
                let updatedInventory = filter (\i -> itemName i /= itemName item) (inventory player)
                let playerWithEffect = applyEffect (effect item) player
                let updatedPlayer = playerWithEffect { armor = Just item, inventory = updatedInventory }
                put state { playerState = updatedPlayer }
                liftIO $ printColored Green $ "You equipped the armor: " ++ itemName item ++ "."
            _ -> liftIO $ printColored Red "This item cannot be equipped."
        Nothing -> liftIO $ printColored Yellow "Item not found."

inspect :: String -> StateT GameState IO ()
inspect itemNameInput = do
    state <- get
    let player = playerState state
    let currentRoom = getPlayerRoom player (world state)

    let maybeItemInInventory = find (\item -> map toLower (itemName item) == map toLower itemNameInput) (inventory player)
    let maybeItemInRoom = find (\item -> map toLower (itemName item) == map toLower itemNameInput) (items currentRoom)

    case maybeItemInInventory <|> maybeItemInRoom of
        Just item -> liftIO $ do
            displayHeader (itemName item)
            putStrLn $ "\n" ++ itemAscii item  -- Display the ASCII art
            putStrLn $ itemDescription item
            putStrLn $ describeEffect (effect item)
            pressEnterToContinue
        Nothing -> liftIO $ do
            printColored Yellow $ "The item \"" ++ itemNameInput ++ "\" is not available to inspect."
            pressEnterToContinue


applyEffect :: Maybe Effect -> Player -> Player
applyEffect Nothing player = player
applyEffect (Just effect) player =
    let -- Apply stat changes
        updatedStats = case modifyStats effect of
            Nothing -> stats player
            Just statChanges -> PlayerStats
                { vitality = max 1 (vitality (stats player) + vitality statChanges)
                , attack = max 1 (attack (stats player) + attack statChanges)
                , defense = max 1 (defense (stats player) + defense statChanges)
                , agility = max 1 (agility (stats player) + agility statChanges)
                }

        -- Apply healing
        updatedLife = case heal effect of
            Nothing -> life player
            Just healingAmount -> min (life player + healingAmount) (vitality updatedStats)

    in player { stats = updatedStats, life = updatedLife }
