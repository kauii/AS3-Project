module Assets.ProgressRelevant.Doors (module Assets.ProgressRelevant.Doors) where

import Types

armoryDoor :: Door
armoryDoor = Door {
    doorName = "Rusted Iron Door",
    leadsTo = "Armory",
    isLocked = True,
    keyRequired = Just "Rusted Key"
}

secretDoor :: Door
secretDoor = Door {
    doorName = "Secret Door",
    leadsTo = "Secret Passage",
    isLocked = True,
    keyRequired = Just "Small Key"
}

-- Sample door
woodenDoor :: Door
woodenDoor = Door
    { doorName = "Wooden Door"
    , leadsTo = "Hallway"
    , isLocked = True
    , keyRequired = Just "Rusty Key"
    }

