module Assets.ProgressRelevant.NPCs (module Assets.ProgressRelevant.NPCs) where

import Types
import Utils.Utils
import Control.Monad.State
import Assets.RandomEntities.RandomizedItems

librarian :: NPC
librarian = NPC {
    npcName = "Librarian",
    requiredItem = Nothing,
    onInteraction = liftIO $ putStrLn "Eternal rest! That sounds delightful!",
    dialogUnavailable = "Eternal rest! That sounds delightful!"
}

blacksmith :: NPC
blacksmith = NPC {
    npcName = "Blacksmith",
    requiredItem = Just "Spoon",
    onInteraction = do
        liftIO $ putStrLn "My beloved spoon! Finally! If you're looking for the dark overlord, just look for the secret passage in the kitchen. It should lead you right to him!"
        gameState <- get
        setGameFlag "passage_detected" True
        let currentWorld = world gameState
        let updatedWorld = addDirectionToRoom "Kitchen" West "Secret Passage" currentWorld

        -- Update the game state
        put gameState { world = updatedWorld },
    dialogUnavailable = "Oh, where is my beloved spoon"
}

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