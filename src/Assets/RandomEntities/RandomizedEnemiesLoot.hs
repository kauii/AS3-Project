module Assets.RandomEntities.RandomizedEnemiesLoot (module Assets.RandomEntities.RandomizedEnemiesLoot) where

import Types

meat :: Item
meat = Item
    { itemName = "Meat"
    , itemDescription = "A chunk of meat that restores some health."
    , effect = Just $ Effect { modifyStats = Nothing, heal = Just 15, unlockDoor = Nothing }
    , quantity = 1
    , itemType = Consumable
    , itemAscii = unlines
        [ "    (    )"
        , "   (______)"
        , "   /      \\"
        , "  |  Meat  |"
        , "   \\______/"
        ]
    }

cape :: Item
cape = Item
    { itemName = "Swift Cape"
    , itemDescription = "A lightweight cape that boosts agility."
    , effect = Just $ Effect { modifyStats = Just PlayerStats { vitality = 0, attack = 0, defense = 0, agility = 5 }, heal = Nothing, unlockDoor = Nothing }
    , quantity = 1
    , itemType = Armor
    , itemAscii = unlines
        [ "   /\\"
        , "  /__\\"
        , "  \\  /"
        , "   \\/"
        ]
    }

skeletonArmor :: Item
skeletonArmor = Item
    { itemName = "Skeleton Armor"
    , itemDescription = "Armor made from bones. Increases defense but reduces agility."
    , effect = Just $ Effect { modifyStats = Just PlayerStats { vitality = 0, attack = 0, defense = 10, agility = -5 }, heal = Nothing, unlockDoor = Nothing }
    , quantity = 1
    , itemType = Armor
    , itemAscii = unlines
        [ "   ||||||"
        , "  |------|"
        , "  |------|"
        , "   ||||||"
        ]
    }

darkKnightArmor :: Item
darkKnightArmor = Item
    { itemName = "Dark Knight Armor"
    , itemDescription = "Heavy armor that offers high defense but drains your health."
    , effect = Just $ Effect { modifyStats = Just PlayerStats { vitality = -5, attack = 0, defense = 20, agility = -10 }, heal = Nothing, unlockDoor = Nothing }
    , quantity = 1
    , itemType = Armor
    , itemAscii = unlines
        [ "   ######"
        , "  #------#"
        , "  #------#"
        , "   ######"
        ]
    }

draconicArmor :: Item
draconicArmor = Item
    { itemName = "Draconic Armor"
    , itemDescription = "Mystical armor that boosts all stats slightly."
    , effect = Just $ Effect { modifyStats = Just PlayerStats { vitality = 5, attack = 0, defense = 10, agility = 5 }, heal = Nothing, unlockDoor = Nothing }
    , quantity = 1
    , itemType = Armor
    , itemAscii = unlines
        [ "  /\\_/\\"
        , " ( O O )"
        , "  > ^ <"
        ]
    }

holySword :: Item
holySword = Item
    { itemName = "Holy Sword"
    , itemDescription = "A blessed sword that increases your attack power."
    , effect = Just $ Effect { modifyStats = Just PlayerStats { vitality = 0, attack = 15, defense = 0, agility = 0 }, heal = Nothing, unlockDoor = Nothing }
    , quantity = 1
    , itemType = Sword
    , itemAscii = unlines
        [ "    /\\"
        , "   /__\\"
        , "    ||"
        , "    ||"
        ]
    }

wyvernFeather :: Item
wyvernFeather = Item
    { itemName = "Wyvern Feather"
    , itemDescription = "A rare feather that boosts all stats."
    , effect = Just $ Effect { modifyStats = Just PlayerStats { vitality = 10, attack = 5, defense = 5, agility = 10 }, heal = Nothing, unlockDoor = Nothing }
    , quantity = 1
    , itemType = Consumable
    , itemAscii = unlines
        [ "    ,~."
        , "   (   )"
        , "   `-^-'"
        ]
    }