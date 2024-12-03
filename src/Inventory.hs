module Inventory (openInventory) where

import Types
import Utils (parseAction)
import Control.Monad.State
import Data.List (intercalate)

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
    let parsedAction = parseAction action

    case parsedAction of
        Just (Drop itemName) -> dropItem itemName >> inventoryInteraction
        Just (UseItem itemName) -> useItem itemName >> inventoryInteraction
        Just (Inspect itemName) -> inspect itemName >> inventoryInteraction
        Just Quit -> liftIO $ putStrLn "Exiting inventory."
        Just _ -> do
            liftIO $ putStrLn "Invalid action, try again."
            inventoryInteraction
        Nothing -> do
            liftIO $ putStrLn "Invalid action, try again."
            inventoryInteraction

dropItem, inspect, useItem ::
    String -> StateT GameState IO ()
dropItem _ = liftIO $ putStrLn "Drop action not implemented yet."
inspect _ = liftIO $ putStrLn "Inspect action not implemented yet."
useItem _ = liftIO $ putStrLn "UseItem action not implemented yet."
