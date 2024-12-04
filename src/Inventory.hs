module Inventory (openInventory) where

import Types
import Utils
import Control.Monad.State
import Data.List (find)
import Data.Char(toLower)
import Data.Maybe(isJust)
import Control.Applicative((<|>))

-- | Display inventory and handle inventory interactions
openInventory :: StateT GameState IO ()
openInventory = do
    state <- get
    let player = playerState state

    -- Display header
    liftIO $ displayHeader "INVENTORY"

    -- Display player stats
    liftIO $ putStrLn "Player Stats:"
    liftIO $ putStrLn $ "  Life: " ++ show (life player) ++ "/" ++ show (vitality (stats player))
    liftIO $ putStrLn $ "  Attack: " ++ show (attack (stats player))
    liftIO $ putStrLn $ "  Defense: " ++ show (defense (stats player))
    liftIO $ putStrLn $ "  Agility: " ++ show (agility (stats player))


    -- Display inventory
    let inventoryItems = inventory player
    liftIO $ if null inventoryItems
        then putStrLn "Your inventory is empty."
        else do
            putStrLn "Inventory:"
            mapM_ (putStrLn . ("  - " ++) . itemName) inventoryItems

    -- Enter inventory interaction
    inventoryInteraction


-- | Handle inventory interaction actions
inventoryInteraction :: StateT GameState IO ()
inventoryInteraction = do
    liftIO $ putStrLn "Available actions: (Drop, Use, Inspect, Back)"
    action <- liftIO getLine
    let parsedAction = parseActionInventory action

    case parsedAction of
        Just (Drop itemName) -> dropItem itemName >> openInventory
        Just (UseItem itemName) -> useItem itemName >> openInventory
        Just (Inspect itemName) -> inspect itemName >> openInventory
        Just Back -> liftIO $ putStrLn "Exiting inventory."
        Just _ -> do
            liftIO $ putStrLn "Invalid action, try again."
            inventoryInteraction
        Nothing -> do
            liftIO $ putStrLn "Invalid action, try again."
            inventoryInteraction


dropItem :: String -> StateT GameState IO ()
dropItem itemNameInput = do
    state <- get
    let player = playerState state
    let currentRoom = getPlayerRoom player (world state)

    -- Find the item in the player's inventory
    let maybeItem = find (\item -> map toLower (itemName item) == map toLower itemNameInput) (inventory player)

    case maybeItem of
        Just item -> do
            -- Remove the item from the player's inventory
            let updatedInventory = filter (\i -> itemName i /= itemName item) (inventory player)
            let updatedPlayer = player { inventory = updatedInventory }

            -- Add the item to the current room
            let updatedRoomItems = item : items currentRoom
            let updatedRoom = currentRoom { items = updatedRoomItems }
            let updatedWorld = map (\room -> if roomName room == roomName currentRoom then updatedRoom else room) (world state)

            -- Update the game state
            put state { playerState = updatedPlayer, world = updatedWorld }
            liftIO $ do
                putStrLn $ "You dropped " ++ itemName item ++ "."
                pressEnterToContinue
        Nothing -> liftIO $ do
            putStrLn "Item not found."
            pressEnterToContinue

inspect :: String -> StateT GameState IO ()
inspect itemNameInput = do
    state <- get
    let player = playerState state
    let currentRoom = getPlayerRoom player (world state)

    -- Check for the item in the inventory
    let maybeItemInInventory = find (\item -> map toLower (itemName item) == map toLower itemNameInput) (inventory player)

    -- Check for the item in the room
    let maybeItemInRoom = find (\item -> map toLower (itemName item) == map toLower itemNameInput) (items currentRoom)

    -- Determine which item to inspect
    case maybeItemInInventory <|> maybeItemInRoom of
        Just item -> liftIO $ do
            displayHeader (itemName item)  -- Header for item name
            putStrLn $ "Description: " ++ itemDescription item
            putStrLn $ describeEffect (effect item)
            pressEnterToContinue
        Nothing -> liftIO $ do
            putStrLn $ "The item \"" ++ itemNameInput ++ "\" is not available to inspect."
            pressEnterToContinue


useItem :: String -> StateT GameState IO ()
useItem itemNameInput = do
    state <- get
    let player = playerState state

    -- Find the item in the player's inventory
    let maybeItem = find (\item -> map toLower (itemName item) == map toLower itemNameInput) (inventory player)

    case maybeItem of
        Just item -> do
            case effect item of
                Nothing -> liftIO $ do
                    putStrLn $ "The item \"" ++ itemName item ++ "\" has no effects and cannot be used."
                    pressEnterToContinue
                Just eff ->
                    if isEffectUsable eff
                        then do
                            -- Apply the effect
                            let newPlayer = applyEffect eff player

                            -- Remove the used item from the inventory
                            let updatedInventory = filter (\i -> itemName i /= itemName item) (inventory player)

                            -- Update the game state
                            put state { playerState = newPlayer { inventory = updatedInventory } }

                            liftIO $ do
                                putStrLn $ "You used the " ++ itemName item ++ "."
                                putStrLn "Effects applied!"
                                pressEnterToContinue
                        else liftIO $ do
                            putStrLn $ "The item \"" ++ itemName item ++ "\" cannot be used."
                            pressEnterToContinue
        Nothing -> liftIO $ do
            putStrLn $ "The item \"" ++ itemNameInput ++ "\" is not in your inventory."
            pressEnterToContinue

isEffectUsable :: Effect -> Bool
isEffectUsable eff = isJust (modifyStats eff) || isJust (heal eff)


applyEffect :: Effect -> Player -> Player
applyEffect effect player =
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


