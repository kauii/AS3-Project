module Assets.ProgressRelevant.NPCs (module Assets.ProgressRelevant.NPCs) where

import Types
import Utils.Utils
import Control.Monad.State
import Assets.RandomEntities.RandomizedItems

-- Example: Quest Giver requiring a "Gold Coin"
questGiver :: NPC
questGiver = NPC {
    npcName = "Quest Giver",
    requiredItem = Just "Gold Coin",
    onInteraction = do
        setGameFlag "quest_completed" True
        liftIO $ putStrLn "Thank you for the Gold Coin! I grant you a Potion."
        addItemToPlayer healthPotion,
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