module Assets.ProgressRelevant.Items (module Assets.ProgressRelevant.Items) where

import Types

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