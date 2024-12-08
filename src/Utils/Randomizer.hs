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

    -- If no valid enemies are found, return the existing enemies
    if null validEnemies
        then return existingEnemies
        else do
            -- Decide how many unique enemies to add based on probabilities
            numEnemies <- weightedEnemyCount

            -- Randomly select unique enemies, ensuring no duplicates are added
            newEnemies <- selectUniqueEnemies validEnemies numEnemies existingEnemies

            return (existingEnemies ++ newEnemies)

-- | Randomly select a specified number of unique enemies from the list, avoiding duplicates.
selectUniqueEnemies :: [Enemy] -> Int -> [Enemy] -> IO [Enemy]
selectUniqueEnemies availableEnemies num existingEnemies = do
    -- Remove enemies already in the room from the available pool
    let filteredEnemies = filter (\e -> not (any (\ex -> enemyName ex == enemyName e) existingEnemies)) availableEnemies

    -- Shuffle the filtered enemies and take the specified number
    shuffledEnemies <- shuffleList filteredEnemies

    return $ take num shuffledEnemies


-- | Function to determine the number of enemies to add based on weighted probabilities.
weightedEnemyCount :: IO Int
weightedEnemyCount = do
    roll <- randomRIO (1 :: Int, 100)
    return $ case roll of
        _ | roll <= 70 -> 1  -- 70% chance for 1 enemy
        _ | roll <= 90 -> 2  -- 20% chance for 2 enemies
        _              -> 3  -- 10% chance for 3 enemies

-- | Randomly select a specified number of unique enemies from the list.
selectUniqueEnemies :: [Enemy] -> Int -> IO [Enemy]
selectUniqueEnemies enemies num = do
    shuffledEnemies <- shuffleList enemies
    return $ take num shuffledEnemies

-- | Helper function to shuffle a list using the Fisher-Yates shuffle.
shuffleList :: [a] -> IO [a]
shuffleList [] = return []
shuffleList xs = do
    randomPositions <- mapM (\i -> randomRIO (0, i)) [length xs - 1, length xs - 2 .. 0]
    return $ foldl swap xs (zip [0..] randomPositions)
  where
    swap list (i, j) = let elemI = list !! i
                           elemJ = list !! j
                       in replaceAt i elemJ $ replaceAt j elemI list

    replaceAt idx newElem list = take idx list ++ [newElem] ++ drop (idx + 1) list

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
