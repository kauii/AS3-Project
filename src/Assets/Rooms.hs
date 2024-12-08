module Assets.Rooms where

import Types

-- ASSETS
import Assets.ProgressRelevant.RoomObjects
import Assets.ProgressRelevant.Doors
import Assets.ProgressRelevant.Items
import Assets.ProgressRelevant.NPCs


-- Starting Room
startingRoom :: Room
startingRoom = Room
    { roomName = "Starting Room"
    , description = "A small, dimly lit room with stone walls."
    , exits = [(North, "Hallway")]
    , roomObjects = []
    , items = []
    , enemies = []
    , doors = []
    , npcs = []
    }

-- Hallway
hallway :: Room
hallway = Room
    { roomName = "Hallway"
    , description = "A long corridor with flickering torches on the walls. You can see another door to the east."
    , exits = [(South, "Starting Room"), (East, "Armory")]
    , roomObjects = []
    , items = []
    , enemies = []
    , doors = []
    , npcs = []
    }

-- Armory
armory :: Room
armory = Room
    { roomName = "Armory"
    , description = "An old armory filled with rusted weapons and armor. A treasure chest lies in the corner."
    , exits = [(West, "Hallway")]
    , roomObjects = []
    , enemies = []
    , doors = []
    , npcs = []
    }
