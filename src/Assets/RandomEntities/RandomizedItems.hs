module Assets.RandomEntities.RandomizedItems (module Assets.RandomEntities.RandomizedItems) where

import Types

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