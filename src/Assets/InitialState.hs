module Assets.InitialState (initialState) where

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
startingRoom = Room {
    roomName = "Starting Room",
    description = "A small, dimly lit room with stone walls.",
    exits = [(North, "Hallway")],
    roomObjects = [],
    items = [healthPotion, rustyKey, goldCoin],
    enemies = [],
    doors = [woodenDoor],
    npcs = [questGiver, villager]
}


hallway :: Room
hallway = Room
    { roomName = "Hallway"
    , description = "A long corridor with flickering torches on the walls. You can see another door to the east."
    , exits = [(South, "Starting Room"), (East, "Armory")]
    , roomObjects = []
    , items = [mysteriousPotion]
    , enemies = [goblin, ghoul]
    , doors = []
    }

armory :: Room
armory = Room
    { roomName = "Armory"
    , description = "An old armory filled with rusted weapons and armor. A treasure chest lies in the corner."
    , exits = [(West, "Hallway")]
    , roomObjects = []
    , items = [woodenSword]
    , enemies = []
    , doors = []
    }

-- Sample items
mysteriousPotion :: Item
mysteriousPotion = Item
    { itemName = "Mysterious Potion"
    , itemDescription = "A strong smelly potion. The liquid is so opaque, you can barely see through. "
    , effect = Just $ Effect { modifyStats = Just PlayerStats 
            { vitality = 10
            , attack = -5
            , defense = 0
            , agility = 0 }
            , heal = Nothing
            , unlockDoor = Nothing }
    , itemType = Consumable
    }


-- Sample enemy
goblin :: Enemy
goblin = Enemy
    { enemyId = Nothing
    , enemyName = "Goblin"
    , enemyHealth = 30
    , enemyMaxHealth = 30
    , enemyAttack = 10
    , enemyDefense = 2
    , enemyAgility = 10
    , loot = [woodenSword]
    , enemyDifficulty = Easy
    }

-- Sample enemy
ghoul :: Enemy
ghoul = Enemy
    { enemyId = Nothing
    , enemyName = "Ghoul"
    , enemyHealth = 25
    , enemyMaxHealth = 25
    , enemyAttack = 5
    , enemyDefense = 10
    , enemyAgility = 21
    , loot = [goldCoin]
    , enemyDifficulty = Easy
    }

goldCoin :: Item
goldCoin = Item
    { itemName = "Gold Coin"
    , itemDescription = "A shiny coin. Valuable for trade."
    , effect = Nothing
    , itemType = KeyItem
    }

-- Sample door
woodenDoor :: Door
woodenDoor = Door
    { doorName = "Wooden Door"
    , leadsTo = "Hallway"
    , isLocked = True
    , keyRequired = Just "Rusty Key"
    }

-- Example: Quest Giver requiring a "Gold Coin"
questGiver :: NPC
questGiver = NPC {
    npcName = "Quest Giver",
    requiredItem = Just "Gold Coin",
    onInteraction = do
        modify (\gs -> gs { flags = Map.insert "quest_completed" True (flags gs) })
        liftIO $ putStrLn "Thank you for the Gold Coin! I grant you a Potion."
        state <- get
        let player = playerState state
        let rewardItem = Item {
            itemName = "Potion",
            itemDescription = "Restores health.",
            effect = Just $ Effect { modifyStats = Nothing, heal = Just 20, unlockDoor = Nothing },
            itemType = Consumable
        }
        let updatedInventory = rewardItem : inventory player
        put state { playerState = player { inventory = updatedInventory } },
    dialogUnavailable = "I could really use a Gold Coin right now. Can you find one?"
}

-- Example: Villager with no requirements
villager :: NPC
villager = NPC {
    npcName = "Villager",
    requiredItem = Nothing,
    onInteraction = liftIO $ putStrLn "Hello, traveler! Stay safe out there.",
    dialogUnavailable = "Hello, traveler! Stay safe out there." -- Same as available dialog
}


-- Initial player state
initialPlayer :: Player
initialPlayer = Player
    { location = "Starting Room"
    , inventory = []
    , life = 90
    , stats = PlayerStats { vitality = 100, attack = 20, defense = 5, agility = 20 }
    , quests = []
    , weapon = Nothing
    , armor = Nothing
    }

-- Initial game state
initialState :: GameState
initialState = GameState
    { playerState = initialPlayer
    , world = [startingRoom, hallway, armory]
    , flags = Map.fromList [("statue_searched", False), ("compartment_opened", False), ("is_true", True)]
    }

healthPotion :: Item
healthPotion = Item
    { itemName = "Health Potion"
    , itemDescription = "A small vial filled with a red liquid. Restores 20 health."
    , effect = Just $ Effect { modifyStats = Nothing, heal = Just 20, unlockDoor = Nothing }
    , itemType = Consumable
    }

rustyKey :: Item
rustyKey = Item
    { itemName = "Rusty Key"
    , itemDescription = "An old, rusty key. It might open a door."
    , effect = Just $ Effect { modifyStats = Nothing, heal = Nothing, unlockDoor = Just "Wooden Door" }
    , itemType = KeyItem
    }

woodenSword :: Item
woodenSword = Item
    { itemName = "Sword"
    , itemDescription = "A sharp blade, perfect for combat."
    , effect = Just $ Effect { modifyStats = Just PlayerStats { vitality = 0, attack = 10, defense = 0, agility = 0 }, heal = Nothing, unlockDoor = Nothing }
    , itemType = Sword
    }

leatherArmor :: Item
leatherArmor = Item
    { itemName = "Leather Armor"
    , itemDescription = "Provides basic protection."
    , effect = Just $ Effect { modifyStats = Just PlayerStats { vitality = 0, attack = 0, defense = 5, agility = -1 }, heal = Nothing, unlockDoor = Nothing }
    , itemType = Armor
    }
