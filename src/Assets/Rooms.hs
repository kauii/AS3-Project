module Assets.Rooms (module Assets.Rooms) where

import Types

-- ASSETS
import Assets.ProgressRelevant.RoomObjects
import Assets.ProgressRelevant.Doors
import Assets.ProgressRelevant.Items
import Assets.ProgressRelevant.NPCs
import Assets.RandomEntities.RandomizedItems


entranceHall :: Room
entranceHall =  Room {
    roomName = "Entrance Hall",
    description = [("A dimly lit stone corridor with a sense of foreboding. Dusty air and faint whispers. To the north there is a door.", "default_true", True)],
    exits = [(North, "Great Hall")],
    roomObjects = [sign],
    items = [],
    enemies = [],
    doors = [],
    npcs = [],
    difficulty = Easy
}

greatHall :: Room
greatHall =  Room {
    roomName = "Great Hall",
    description = [("A vast chamber with a crumbling chandelier overhead. Doors lead in all directions.", "default_true", True)],
    exits = [(North, "Library"),(East, "Dining Room"),(South, "Entrance Hall"),(West, "Armory")],
    roomObjects = [chandelier],
    items = [],
    enemies = [],
    doors = [armoryDoor],
    npcs = [],
    difficulty = Easy
}

library :: Room
library =  Room {
    roomName = "Library",
    description = [("Shelves filled with decaying tomes. A faintly glowing chest in the corner. A librarian ghost reading in a book. To the south there is a door.", "default_true", True)],
    exits = [(South, "Great Hall")],
    roomObjects = [bookshelve, chest],
    items = [],
    enemies = [],
    doors = [],
    npcs = [librarian],
    difficulty = Easy
}

diningRoom :: Room
diningRoom =  Room {
    roomName = "Dining Room",
    description = [("A long table with rotting food and broken candlesticks. There is a door to the west and a staircase leading downwards.", "default_true", True)],
    exits = [(West, "Great Hall"), (Down, "Kitchen")],
    roomObjects = [portrait],
    items = [candle],
    enemies = [],
    doors = [],
    npcs = [],
    difficulty = Easy
}

kitchen :: Room
kitchen =  Room {
    roomName = "Kitchen",
    description = [("A filthy room with cobwebbed cabinets and an unlit stove. There is a staircase leading upwards.", "passage_detected", False),
                   ("A filthy room with cobwebbed cabinets and an unlit stove. There is a staircase leading upwards and a secret door to the west.", "passage_detected", True)],
    exits = [(Up, "Dining Room")],
    roomObjects = [],
    items = [spoon],
    enemies = [],
    doors = [secretDoor],
    npcs = [],
    difficulty = Easy
}

secretPassage :: Room
secretPassage =  Room {
    roomName = "Secret Passage",
    description = [("Narrow, dark, with uneven walls. There are doors to the north and east.", "default_true", True)],
    exits = [(North, "Crypt"), (East, "Kitchen")],
    roomObjects = [],
    items = [],
    enemies = [],
    doors = [],
    npcs = [],
    difficulty = Hard
}

armory :: Room
armory =  Room {
    roomName = "Armory",
    description = [("Rusted weapon racks and a locked cabinet. A faint spectral glow lingers. There is a door to the east and a staircase leading downwards. A blacksmith ghost is wandering through the room.", "default_true", True)],
    exits = [(East, "Great Hall"), (Down, "Forge")],
    roomObjects = [cabinet],
    items = [],
    enemies = [],
    doors = [],
    npcs = [blacksmith],
    difficulty = Normal
}

forge :: Room
forge =  Room {
    roomName = "Forge",
    description = [("A blazing forge and glowing runes on an anvil.", "default_true", True)],
    exits = [(Up, "Armory")],
    roomObjects = [anvil],
    items = [],
    enemies = [],
    doors = [],
    npcs = [],
    difficulty = Normal
}

-- A sample room
startingRoom :: Room
startingRoom = Room {
    roomName = "Starting Room",
    description = [("A small, dimly lit room with stone walls.", "default_true", True)],
    exits = [(North, "Hallway")],
    roomObjects = [],
    items = [healthPotion, rustyKey, goldCoin],
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
    , enemies = []
    , doors = []
    , npcs = []
    , difficulty = Easy
    }

crypt :: Room
crypt = Room
    { roomName = "Crypt"
    , description = [ ("The air is damp and echoing, carrying the whispers of ancient secrets." 
    ++"\nThree ancient switches are set into the wall, their purpose shrouded in mystery.", "default_true", True)]
    , exits = [(South, "Secret Passage"), (Down, "Ancient Valley")]
    , roomObjects = [switches]
    , items = []
    , enemies = []
    , doors = [ancientSeal]
    , npcs = []
    , difficulty = Hard
    }
