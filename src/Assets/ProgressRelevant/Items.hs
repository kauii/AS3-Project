module Assets.ProgressRelevant.Items (module Assets.ProgressRelevant.Items) where

import Types

rustedKey :: Item
rustedKey = Item
    { itemName = "Rusted Key"
    , itemDescription = "An old, rusted key. It might open a door."
    , effect = Just $ Effect { modifyStats = Nothing, heal = Nothing, unlockDoor = Just "Rusted Iron Door" }
    , quantity = 1
    , itemType = KeyItem
    , itemAscii = ""
    }

smallKey :: Item
smallKey = Item {
    itemName = "Small Key",
    itemDescription = "An very small key.",
    effect = Just $ Effect { modifyStats = Nothing, heal = Nothing, unlockDoor = Just "Secret Passage" },
    quantity = 1,
    itemType = KeyItem,
    itemAscii = ""
}

silverKey :: Item
silverKey = Item {
    itemName = "Silver Key",
    itemDescription = "A shiny, silver key.",
    effect = Nothing,
    quantity = 1,
    itemType = KeyItem,
    itemAscii = ""
}

candle :: Item
candle = Item {
    itemName = "Candle",
    itemDescription = "An ordinary candle.",
    effect = Nothing,
    quantity = 1,
    itemType = KeyItem,
    itemAscii = ""
}

spoon :: Item
spoon = Item {
    itemName = "Spoon",
    itemDescription = "An silver spoon.",
    effect = Nothing,
    quantity = 1,
    itemType = KeyItem,
    itemAscii = ""
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
    , quantity = 1
    , itemType = Consumable
    , itemAscii = ""
    }

rustyKey :: Item
rustyKey = Item
    { itemName = "Rusty Key"
    , itemDescription = "An old, rusty key. It might open a door."
    , effect = Just $ Effect { modifyStats = Nothing, heal = Nothing, unlockDoor = Just "Wooden Door" }
    , quantity = 1
    , itemType = KeyItem
    , itemAscii = ""
    }

woodenSword :: Item
woodenSword = Item
    { itemName = "Wooden Sword"
    , itemDescription = "A sharp blade, perfect for combat."
    , effect = Just $ Effect { modifyStats = Just PlayerStats { vitality = 0, attack = 10, defense = 0, agility = 0 }, heal = Nothing, unlockDoor = Nothing }
    , quantity = 1
    , itemType = Sword
    , itemAscii = ""
    }

leatherArmor :: Item
leatherArmor = Item
    { itemName = "Leather Armor"
    , itemDescription = "Provides basic protection."
    , effect = Just $ Effect { modifyStats = Just PlayerStats { vitality = 0, attack = 0, defense = 5, agility = -1 }, heal = Nothing, unlockDoor = Nothing }
    , quantity = 1
    , itemType = Armor
    , itemAscii = ""
    }

goldCoin :: Item
goldCoin = Item
    { itemName = "Gold Coin"
    , itemDescription = "A shiny coin. Valuable for trade."
    , effect = Nothing
    , quantity = 1
    , itemType = KeyItem
    , itemAscii = ""
    }

ancientKey :: Item
ancientKey = Item
    { itemName = "Ancient Key"
    , itemDescription = "An ancient key, forged to open the seal to the long forgotten Valley."
    , effect = Nothing
    , quantity = 1
    , itemType = KeyItem
    , itemAscii = unlines
        [ "       ____"
        , "      |    |"
        , "      |____|"
        , "        ||"
        , "        ||"
        , "        ||"
        , "       /__\\"
        ]
    }