module Assets.ProgressRelevant.Doors (module Assets.ProgressRelevant.Doors) where

import Types

-- Sample door
woodenDoor :: Door
woodenDoor = Door
    { doorName = "Wooden Door"
    , leadsTo = "Hallway"
    , isLocked = True
    , keyRequired = Just "Rusty Key"
    }