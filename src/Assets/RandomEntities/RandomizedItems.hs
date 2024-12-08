module Assets.RandomEntities.RandomizedItems (module Assets.RandomEntities.RandomizedItems) where

import Types

randomizedItems :: [Item]
randomizedItems = [healthPotion, mysteriousPotionRandom, strengthElixir, enchantedHerb, speedBerry, strongHealingPotion]
 
healthPotion :: Item
healthPotion = Item
    { itemName = "Health Potion"
    , itemDescription = "A small vial filled with a red liquid. Restores 20 health."
    , effect = Just $ Effect { modifyStats = Nothing, heal = Just 20, unlockDoor = Nothing }
    , quantity = 1
    , itemType = Consumable
    , itemAscii = unlines
    [ "     _____"
    , "    |_____|"
    , "    /     \\"
    , "   /       \\"
    , "  |   ***   |"
    , "   \\  ***  /"
    , "    \\_____/"
    ]
    }

mysteriousPotionRandom :: Item
mysteriousPotionRandom = Item
    { itemName = "Mysterious Potion"
    , itemDescription = "A strong-smelling potion with an opaque liquid. Effects are unpredictable."
    , effect = Just $ Effect { modifyStats = Just PlayerStats { vitality = 10, attack = -5, defense = 0, agility = 0 }, heal = Just (-20), unlockDoor = Nothing }
    , quantity = 1
    , itemType = Consumable
    , itemAscii = unlines
        [ "     _____"
        , "    |?????|"
        , "    / ??? \\"
        , "   /       \\"
        , "  |   ???   |"
        , "   \\  ???  /"
        , "    \\_____/"
        ]
    }

strengthElixir :: Item
strengthElixir = Item
    { itemName = "Strength Elixir"
    , itemDescription = "A potent brew that temporarily increases strength."
    , effect = Just $ Effect { modifyStats = Just PlayerStats { vitality = 0, attack = 15, defense = 0, agility = 0 }, heal = Nothing, unlockDoor = Nothing }
    , quantity = 1
    , itemType = Consumable
    , itemAscii = unlines
        [ "     _____"
        , "    |:::::|"
        , "    /     \\"
        , "   /       \\"
        , "  | STRNGTH |"
        , "   \\       /"
        , "    \\_____/"
        ]
    }

enchantedHerb :: Item
enchantedHerb = Item
    { itemName = "Enchanted Herb"
    , itemDescription = "A mystical herb that restores agility."
    , effect = Just $ Effect { modifyStats = Just PlayerStats { vitality = 2, attack = 2, defense = 2, agility = 2 }, heal = Nothing, unlockDoor = Nothing }
    , quantity = 1
    , itemType = Consumable
    , itemAscii = unlines
        [ "    \\|/"
        , "   --*--"
        , "    /|\\"
        ]
    }

speedBerry :: Item
speedBerry = Item
    { itemName = "Speed Berry"
    , itemDescription = "A rare berry that temporarily boosts speed."
    , effect = Just $ Effect { modifyStats = Just PlayerStats { vitality = 0, attack = 0, defense = 0, agility = 10 }, heal = Nothing, unlockDoor = Nothing }
    , quantity = 1
    , itemType = Consumable
    , itemAscii = unlines
        [ "     ___"
        , "    /   \\"
        , "   | *** |"
        , "    \\___/"
        ]
    }

strongHealingPotion :: Item
strongHealingPotion = Item
    { itemName = "Strong Healing Potion"
    , itemDescription = "A potent potion that heals greatly, but at a cost to your strength and reflexes."
    , effect = Just $ Effect 
        { modifyStats = Just PlayerStats 
            { vitality = 0
            , attack = -3
            , defense = -3
            , agility = -3
            }
        , heal = Just 100
        , unlockDoor = Nothing
        }
    , quantity = 1
    , itemType = Consumable
    , itemAscii = unlines
        [ "      ______"
        , "     |      |"
        , "     | !!!! |"
        , "     |______|"
        , "     |STRONG|"
        , "     \\______/ "
        ]
    }
