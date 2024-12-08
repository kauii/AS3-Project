module Inventory (openInventory) where

import Types
import Utils.Utils
import Control.Monad.State
import Control.Monad(when)
import Data.List (find)
import Data.Char(toLower)
import Data.Maybe(isJust)
import Control.Applicative((<|>))

openInventory :: Bool -> StateT GameState IO Player
openInventory inCombat = do
    state <- get
    let player = playerState state

    liftIO $ displayHeader "INVENTORY"
    liftIO $ putStrLn "Player Stats:"
    liftIO $ putStrLn $ "  Life: " ++ show (life player) ++ "/" ++ show (vitality (stats player))
    liftIO $ putStrLn $ "  Attack: " ++ show (attack (stats player))
    liftIO $ putStrLn $ "  Defense: " ++ show (defense (stats player))
    liftIO $ putStrLn $ "  Agility: " ++ show (agility (stats player))

    let inventoryItems = inventory player
    liftIO $ if null inventoryItems
        then putStrLn "Your inventory is empty."
        else mapM_ (putStrLn . ("  - " ++) . itemName) inventoryItems
    inventoryInteraction inCombat player

inventoryInteraction :: Bool -> Player -> StateT GameState IO Player
inventoryInteraction inCombat player = do
    let availableActions = if inCombat then "Available actions: (Use, Inspect, Back)" else "Available actions: (Drop, Use, Inspect, Back)"
    liftIO $ putStrLn availableActions
    action <- liftIO getLine
    let parsedAction = parseActionInventory action

    case parsedAction of
        Just (Drop itemName)
            | not inCombat -> dropItem itemName >> openInventory inCombat
            | otherwise -> restrictedAction "Drop" >> inventoryInteraction inCombat player
        Just (UseItem itemName) -> do
            useItem itemName inCombat
            if inCombat then getPlayerState else openInventory inCombat
        Just (Inspect itemName) -> do
            inspect itemName
            inventoryInteraction inCombat player
        Just Back -> return player
        _ -> do
            liftIO $ putStrLn "Invalid action, try again."
            inventoryInteraction inCombat player

restrictedAction :: String -> StateT GameState IO ()
restrictedAction action = liftIO $ putStrLn $ action ++ " is not allowed in combat!"

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

            let updatedRoomItems = item : items currentRoom
            let updatedRoom = currentRoom { items = updatedRoomItems }
            let updatedWorld = map (\room -> if roomName room == roomName currentRoom then updatedRoom else room) (world state)

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

    let maybeItemInInventory = find (\item -> map toLower (itemName item) == map toLower itemNameInput) (inventory player)

    let maybeItemInRoom = find (\item -> map toLower (itemName item) == map toLower itemNameInput) (items currentRoom)

    case maybeItemInInventory <|> maybeItemInRoom of
        Just item -> liftIO $ do
            displayHeader (itemName item)
            putStrLn $ "Description: " ++ itemDescription item
            putStrLn $ describeEffect (effect item)
            pressEnterToContinue
        Nothing -> liftIO $ do
            putStrLn $ "The item \"" ++ itemNameInput ++ "\" is not available to inspect."
            pressEnterToContinue

useItem :: String -> Bool -> StateT GameState IO ()
useItem itemNameInput inCombat = do
    state <- get
    let player = playerState state
    let maybeItem = find (\item -> map toLower (itemName item) == map toLower itemNameInput) (inventory player)

    case maybeItem of
        Just item -> do
            case effect item of
                Nothing -> liftIO $ putStrLn $ "The item \"" ++ itemName item ++ "\" has no effects and cannot be used."
                Just eff ->
                    if isEffectUsable eff
                        then do
                            let newPlayer = applyEffect eff player
                            let updatedInventory = filter (\i -> itemName i /= itemName item) (inventory newPlayer)
                            let finalPlayer = newPlayer { inventory = updatedInventory }
                            put state { playerState = finalPlayer }
                            when inCombat $ setTurnEnded True
                            liftIO $ do
                                putStrLn $ "You used the " ++ itemName item ++ "."
                                putStrLn "Effects applied!"
                        else liftIO $ putStrLn $ "The item \"" ++ itemName item ++ "\" cannot be used."
        Nothing -> liftIO $ putStrLn $ "The item \"" ++ itemNameInput ++ "\" is not in your inventory."

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

getPlayerState :: StateT GameState IO Player
getPlayerState = do
    playerState <$> get