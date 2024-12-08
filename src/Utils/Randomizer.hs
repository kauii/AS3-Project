module Utils.Randomizer (randomizeGameState) where

import Types
import Assets.RandomEntities.RandomizedItems (randomizedItems)
import Assets.RandomEntities.RandomizedEnemies (randomizedEnemies)
import Control.Monad.State
import System.Random (randomRIO)
import Data.List (partition)
import Control.Monad (replicateM)

-- | Randomize the game state by adding random items and enemies to the rooms.
randomizeGameState :: GameState -> IO GameState
randomizeGameState gameState = do
    updatedRooms <- mapM randomizeRoom (world gameState)
    return gameState { world = updatedRooms }

-- | Randomize items and enemies in a room.
randomizeRoom :: Room -> IO Room
randomizeRoom room = do
    -- Randomize items with a 50% chance
    addItems <- randomChance 50
    newItems <- if addItems
                then randomizeItems (items room)
                else return (items room)

    -- Randomize enemies with a 40% chance
    addEnemies <- randomChance 40
    newEnemies <- if addEnemies
                  then randomizeEnemies (enemies room) (difficulty room)
                  else return (enemies room)

    return room { items = newItems, enemies = newEnemies }

-- | Randomize items and add to the existing list.
randomizeItems :: [Item] -> IO [Item]
randomizeItems existingItems = do
    numItems <- randomRIO (1, 3)  -- Random number of items to add (1 to 3)
    selectedItems <- replicateM numItems (randomChoice randomizedItems)
    let updatedItems = foldl addOrUpdateItem existingItems selectedItems
    return updatedItems

-- | Randomize enemies matching the room difficulty and add to the existing list.
randomizeEnemies :: [Enemy] -> Difficulty -> IO [Enemy]
randomizeEnemies existingEnemies roomDifficulty = do
    -- Filter enemies matching the room difficulty
    let validEnemies = filter (\e -> enemyDifficulty e == roomDifficulty) randomizedEnemies
    if null validEnemies
        then return existingEnemies
        else do
            numEnemies <- randomRIO (1, 2)  -- Randomly add 1 or 2 enemies
            newEnemies <- replicateM numEnemies (randomChoice validEnemies)
            return (existingEnemies ++ newEnemies)

-- | Helper function to randomly decide based on a percentage chance.
randomChance :: Int -> IO Bool
randomChance percent = do
    roll <- randomRIO (1, 100)
    return (roll <= percent)

-- | Randomly choose an element from a list.
randomChoice :: [a] -> IO a
randomChoice xs = do
    idx <- randomRIO (0, length xs - 1)
    return (xs !! idx)

-- | Add an item to the list or update its quantity if it already exists.
addOrUpdateItem :: [Item] -> Item -> [Item]
addOrUpdateItem [] newItem = [newItem]
addOrUpdateItem (x:xs) newItem
    | itemName x == itemName newItem = x { quantity = quantity x + quantity newItem } : xs
    | otherwise = x : addOrUpdateItem xs newItem
