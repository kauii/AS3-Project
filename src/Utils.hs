module Utils (parseAction, parseDirection) where

import Types

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
