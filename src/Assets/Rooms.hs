module Assets.Rooms (module Assets.Rooms) where

import Types

-- ASSETS
import Assets.ProgressRelevant.RoomObjects
import Assets.ProgressRelevant.Doors
import Assets.ProgressRelevant.Items
import Assets.ProgressRelevant.NPCs
import Assets.RandomEntities.RandomizedItems
import Assets.RandomEntities.RandomizedEnemies



-- A sample room
startingRoom :: Room
startingRoom = Room {
    roomName = "Starting Room",
    description = [("A small, dimly lit room with stone walls.", "default_true", True)],
    exits = [(North, "Hallway")],
    roomObjects = [],
    items = [healthPotion, rustyKey, goldCoin, mysteriousPotion, strengthElixir, enchantedHerb, speedBerry, strongHealingPotion],
    enemies = [],
    doors = [woodenDoor],
    npcs = [questGiver, villager],
    difficulty = Easy
}


hallway :: Room
hallway = Room
    { roomName = "Hallway"
    , description = [("A long corridor with flickering torches on the walls. You can see another door to the east.", "default_true", True)]
    , exits = [(South, "Starting Room"), (East, "Armory")]
    , roomObjects = []
    , items = [mysteriousPotion]
    , enemies = [goblin, rat, skeleton]
    , doors = []
    , npcs = []
    , difficulty = Easy
    }

armory :: Room
armory = Room
    { roomName = "Armory"
    , description = [("An old armory filled with rusted weapons and armor. A treasure chest lies in the corner.", "default_true", True)]
    , exits = [(West, "Hallway")]
    , roomObjects = []
    , items = [woodenSword]
    , enemies = []
    , doors = []
    , npcs = []
    , difficulty = Easy
    }