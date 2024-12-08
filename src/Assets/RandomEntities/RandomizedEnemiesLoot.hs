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

dagger :: Item
dagger = Item
    { itemName = "Dagger"
    , itemDescription = "A sharp weapon often used by bandits and goblins to steal gold from the wealthy."
    , effect = Just $ Effect
        { modifyStats = Just $ PlayerStats
            { vitality = 0
            , attack = 5    -- Increases attack by 5
            , defense = 0
            , agility = 3   -- Increases agility by 3
            }
        , heal = Nothing
        , unlockDoor = Nothing
        }
    , quantity = 1
    , itemType = Sword
    , itemAscii = unlines
        [ "    /\\"
        , "   /  \\"
        , "  /____\\"
        , "    ||"
        , "    ||"
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
    [ "      .-.     "
    , "     (o_o)    "
    , "    __|=|__   "
    , "   // | | \\\\ "
    , "  //  | |  \\\\"
    , "  ||  |_|  ||"
    , "   \\__|__/  "
    , "     |_|_|   "
    , "     /_|_\\   "
    ]
    }

darkKnightAxe :: Item
darkKnightAxe = Item
    { itemName = "Dark Knight Axe"
    , itemDescription = "A heavy axe forged in darkness. Increases attack significantly."
    , effect = Just $ Effect
        { modifyStats = Just PlayerStats
            { vitality = 0
            , attack = 15
            , defense = 0
            , agility = -2
            }
        , heal = Nothing
        , unlockDoor = Nothing
        }
    , quantity = 1
    , itemType = Sword 
    , itemAscii = unlines
        [ " _________________.---.______"
        , "(_(______________(_o o_(____()"
        , "        mrf  .___.'. .'.___."
        , "             \\ o    Y    o /"
        , "              \\ \\__   __/ /"
        , "               '.__'-'__.'"
        , "                   '''"
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
    [ "       ! "
    , "      .-. "
    , "    __|=|__"
    , "   (_/`-`\\_)"
    , "   //\\___/\\\\"
    , "   <>/   \\<>"
    , "    \\|_._|/"
    , "     <_I_>"
    , "      |||"
    , "     /_|_\\"
    ]
    }

draconicArmor :: Item
draconicArmor = Item
    { itemName = "Draconic Armor"
    , itemDescription = "Mystical armor that boosts all stats slightly."
    , effect = Just $ Effect { modifyStats = Just PlayerStats { vitality = 5, attack = 0, defense = 15, agility = 5 }, heal = Nothing, unlockDoor = Nothing }
    , quantity = 1
    , itemType = Armor
    , itemAscii = unlines
    [ "      /\\    "
    , "     /__\\   "
    , "    |\\  /|  "
    , "   __|\\/|__ "
    , "  (/  <>  \\)"
    , "   //\\___/\\\\"
    , "   <>/   \\<>"
    , "    \\|_._|/"
    , "     <_I_>"
    , "     /|||\\"
    , "    /_|_|_\\"
    ]
    }

holySword :: Item
holySword = Item
    { itemName = "Holy Sword"
    , itemDescription = "A blessed sword that increases your attack power."
    , effect = Just $ Effect { modifyStats = Just PlayerStats { vitality = 4, attack = 25, defense = 0, agility = 0 }, heal = Nothing, unlockDoor = Nothing }
    , quantity = 1
    , itemType = Sword
    , itemAscii = unlines
    [ "      /| ________________"
    , "O|===|* >________________>"
    , "      \\|"
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
    [ "   ,"
    , "  /|"
    , " / |"
    , " | |"
    , " \\ |"
    , "  \\|"
    , "   '"
    ]
    }