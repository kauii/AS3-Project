module Utils (parseAction, parseDirection, getPlayerRoom, findRoom, parseActionInventory, checkFlag) where

import Types
import Data.Maybe (fromMaybe)
import Data.List(find)
import qualified Data.Map as Map

-- | Parse the user input into an Action
parseAction :: String -> Maybe Action
parseAction input =
    case words input of
        ["Go", dir] -> fmap Go (parseDirection dir)
        ("Take" : itemWords) -> Just (Take (unwords itemWords))
        ("Drop" : itemWords) -> Just (Drop (unwords itemWords))
        ("Inspect" : nameWords) -> Just (Inspect (unwords nameWords))
        ("Attack" : nameWords) -> Just (Attack (unwords nameWords))
        ("TalkTo" : nameWords) -> Just (TalkTo (unwords nameWords))
        ("OpenDoor" : nameWords) -> Just (OpenDoor (unwords nameWords))
        ("UseItem" : nameWords) -> Just (UseItem (unwords nameWords))
        ["OpenInv"] -> Just OpenInv
        ["Quit"] -> Just Quit
        _ -> Nothing

-- | Parse a direction string into a Direction
parseDirection :: String -> Maybe Direction
parseDirection dir = case dir of
    "North" -> Just North
    "South" -> Just South
    "East"  -> Just East
    "West"  -> Just West
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
    case words input of
        ("Drop" : itemWords) -> Just (Drop (unwords itemWords))
        ("Inspect" : nameWords) -> Just (Inspect (unwords nameWords))
        ("UseItem" : nameWords) -> Just (UseItem (unwords nameWords))
        ["Back"] -> Just Back
        _ -> Nothing

-- Function to check if a flag condition is met
checkFlag :: String -> Bool -> Map.Map String Bool -> Bool
checkFlag key requiredState flags =
    case Map.lookup key flags of
        Just state -> state == requiredState
        Nothing    -> not requiredState  -- Default to False if flag not found