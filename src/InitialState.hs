module InitialState (initialState) where

import Types
import Control.Monad.State
import qualified Data.Map as Map

object1 :: RoomObject
object1 = RoomObject {
    objectName = "Mysterious Statue",
    descriptions = [
        ("An ancient statue covered in moss.", "is_true", True),
        ("The statue has a hidden compartment.", "statue_searched", True),
        ("The compartment is open and empty", "compartment_opened", True)
    ],
    roomObjectItems = [],
    roomActions = [
        ("search", \_ -> do
            modify (\gs -> gs { flags = Map.insert "statue_searched" True (flags gs) })
            liftIO $ putStrLn "You discover a hidden compartment in the statue!"),
        ("open", \_ -> do
            modify (\gs -> gs { flags = Map.insert "compartment_opened" True (flags gs) })
            liftIO $ putStrLn "You discover a hidden compartment in the statue!")
    ]
}

-- A sample room
startingRoom :: Room
startingRoom = Room
    { roomName = "Starting Room"
    , description = "A small, dimly lit room with stone walls. There's a wooden door to the north."
    , exits = [(North, "Hallway")]
    , roomObjects = [object1]
    , items = [healthPotion, rustyKey]
    , enemies = []
    , doors = [woodenDoor]
    }

hallway :: Room
hallway = Room
    { roomName = "Hallway"
    , description = "A long corridor with flickering torches on the walls. You can see another door to the east."
    , exits = [(South, "Starting Room"), (East, "Armory")]
    , roomObjects = []
    , items = []
    , enemies = [goblin]
    , doors = []
    }

armory :: Room
armory = Room
    { roomName = "Armory"
    , description = "An old armory filled with rusted weapons and armor. A treasure chest lies in the corner."
    , exits = [(West, "Hallway")]
    , roomObjects = []
    , items = [sword]
    , enemies = []
    , doors = []
    }

-- Sample items
healthPotion :: Item
healthPotion = Item
    { itemName = "Health Potion"
    , itemDescription = "A small vial filled with a red liquid. Restores 20 health."
    , effect = Just $ Effect { modifyStats = Nothing, heal = Just 20, unlockDoor = Nothing }
    }

rustyKey :: Item
rustyKey = Item
    { itemName = "Rusty Key"
    , itemDescription = "An old, rusty key. It might open a door."
    , effect = Just $ Effect { modifyStats = Nothing, heal = Nothing, unlockDoor = Just "Wooden Door" }
    }

sword :: Item
sword = Item
    { itemName = "Sword"
    , itemDescription = "A sharp blade, perfect for combat."
    , effect = Nothing
    }

-- Sample enemy
goblin :: Enemy
goblin = Enemy
    { enemyName = "Goblin"
    , enemyHealth = 30
    , enemyAttack = 5
    , enemyDefense = 2
    , loot = [goldCoin]
    }

goldCoin :: Item
goldCoin = Item
    { itemName = "Gold Coin"
    , itemDescription = "A shiny coin. Valuable for trade."
    , effect = Nothing
    }

-- Sample door
woodenDoor :: Door
woodenDoor = Door
    { doorName = "Wooden Door"
    , leadsTo = "Hallway"
    , isLocked = True
    , keyRequired = Just "Rusty Key"
    }

-- Initial player state
initialPlayer :: Player
initialPlayer = Player
    { location = "Starting Room"
    , inventory = []
    , life = 100
    , maxLife = 100
    , stats = PlayerStats { attack = 10, defense = 5 }
    , quests = []
    }

-- Initial game state
initialState :: GameState
initialState = GameState
    { playerState = initialPlayer
    , world = [startingRoom, hallway, armory]
    , flags = Map.fromList [("statue_searched", False), ("compartment_opened", False), ("is_true", True)]
    }