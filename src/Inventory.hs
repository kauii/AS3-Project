module Inventory (openInventory) where

import Types
import Utils (parseActionInventory, getPlayerRoom, findRoom)
import Control.Monad.State
import Data.List (find, intercalate)
import Data.Char(toLower)

-- | Display inventory and handle inventory interactions
openInventory :: StateT GameState IO ()
openInventory = do
    state <- get
    let player = playerState state

    -- Display player stats
    liftIO $ putStrLn "Player Stats:"
    liftIO $ putStrLn $ "  Life: " ++ show (life player) ++ "/" ++ show (maxLife player)
    liftIO $ putStrLn $ "  Attack: " ++ show (attack (stats player))
    liftIO $ putStrLn $ "  Defense: " ++ show (defense (stats player))

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
        Just (Drop itemName) -> dropItem itemName >> inventoryInteraction
        Just (UseItem itemName) -> useItem itemName >> inventoryInteraction
        Just (Inspect itemName) -> inspect itemName >> inventoryInteraction
        Just Back -> liftIO $ putStrLn "Exiting inventory."
        Just _ -> do
            liftIO $ putStrLn "Invalid action, try again."
            inventoryInteraction
        Nothing -> do
            liftIO $ putStrLn "Invalid action, try again."
            inventoryInteraction

inspect, useItem ::
    String -> StateT GameState IO ()
inspect _ = liftIO $ putStrLn "Inspect action not implemented yet."
useItem _ = liftIO $ putStrLn "UseItem action not implemented yet."

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
            liftIO $ putStrLn $ "You dropped " ++ itemName item ++ "."
        Nothing -> liftIO $ putStrLn "Item not found."