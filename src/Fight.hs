module Fight (enterCombat) where

import Data.List(find, sortOn)
import Data.Maybe(fromJust)
import Inventory
import Types
import Utils.Utils
import Control.Monad.State
import System.Random (randomRIO)

enterCombat :: StateT GameState IO ()
enterCombat = do
    
    state <- get
    let player = playerState state
    let currentRoom = getPlayerRoom player (world state)
    let enemiesInRoom = enemies currentRoom

    if null enemiesInRoom
        then liftIO $ putStrLn "The room is peaceful. No enemies here."
        else do
            liftIO $ displayHeader "Combat"
            liftIO $ putStrLn "Enemies spotted!\n"
            
            combatLoop player enemiesInRoom


combatLoop :: Player -> [Enemy] -> StateT GameState IO ()
combatLoop player enemiesCurrent = do
    state <- get
    let currentRoom = getPlayerRoom (playerState state) (world state)

    if null enemiesCurrent
        then do
            -- Collect loot from defeated enemies
            let defeatedLoot = collectLoot (enemies currentRoom)
                updatedRoom = currentRoom 
                              { enemies = []  -- Remove enemies from the room
                              , items = items currentRoom ++ defeatedLoot -- Add loot to room's items
                              }
                updatedWorld = map (\room -> if roomName room == roomName currentRoom then updatedRoom else room) (world state)

            -- Update the game state
            put state { world = updatedWorld }

            -- Notify the player
            liftIO $ putStrLn "\nAll enemies defeated! Loot has been added to the room:"
            liftIO $ mapM_ (putStrLn . ("  - " ++) . itemName) defeatedLoot
            
            lift pressEnterToContinue
        else if life player <= 0
            then do
                liftIO $ putStrLn "\nYou have been defeated..."
                updatePlayerState player -- Update player's state to reflect death
                lift pressEnterToContinue
        else do
            lift pressEnterToContinue
            -- Print health bars for both player and enemies
            liftIO $ printCombatHealthBars enemiesCurrent player
            -- Determine turn order based on agility
            let combatants = determineTurnOrder player enemiesCurrent
            -- Process turns
            processTurns combatants player enemiesCurrent



updatePlayerState :: Player -> StateT GameState IO ()
updatePlayerState updatedPlayer = do
    state <- get
    put state { playerState = updatedPlayer }

-- | Determine turn order based on agility
determineTurnOrder :: Player -> [Enemy] -> [(String, Int, Maybe Enemy)]
determineTurnOrder player enemies =
    let playerAgility = agility (stats player)
        enemiesAgility = map (\e -> (enemyName e, enemyAgility e, Just e)) enemies
    in reverse $ sortOn (\(_, ag, _) -> ag) (("Player", playerAgility, Nothing) : enemiesAgility)

processTurns :: [(String, Int, Maybe Enemy)] -> Player -> [Enemy] -> StateT GameState IO ()
processTurns [] player enemies = combatLoop player enemies  -- Recurse back into the combat loop when all turns are processed
processTurns ((name, _, Nothing) : rest) player enemies = do  -- Player's turn
    setTurnEnded False
    liftIO $ putStrLn $ "\n" ++ name ++ "'s turn (Player)"
    liftIO $ putStrLn "Actions available: (Attack, OpenInv, Flee)"
    action <- liftIO getLine
    let parsedAction = parseActionFight action
    case parsedAction of
        Just Attack -> do
            updatedEnemies <- attackEnemy player enemies
            -- Filter out defeated enemies from the remaining combatants
            let filteredRest = filter (\(_, _, maybeEnemy) -> maybeEnemy == Nothing || (maybeEnemy /= Nothing && enemyHealth (fromJust maybeEnemy) > 0)) rest
            processTurns filteredRest player updatedEnemies
        Just OpenInv -> do
            updatedPlayer <- openInventory True
            turnEnded <- isTurnEnded
            if turnEnded
                then processTurns rest updatedPlayer enemies
                else processTurns ((name, agility (stats updatedPlayer), Nothing) : rest) updatedPlayer enemies
        Just Flee -> attemptFlee player enemies rest  -- Handle flee action
        _ -> do
            liftIO $ putStrLn "Invalid action. Try again."
            processTurns ((name, agility (stats player), Nothing) : rest) player enemies
processTurns ((name, _, Just enemy) : rest) player enemies = do  -- Enemy's turn
    -- Check if the enemy is still alive before proceeding
    let maybeEnemy = find (\e -> enemyName e == enemyName enemy) enemies
    case maybeEnemy of
        Just aliveEnemy -> do
            liftIO $ putStrLn $ "\n" ++ name ++ "'s turn (Enemy)"
            updatedPlayer <- enemyTurn player aliveEnemy
            updatePlayerState updatedPlayer
            processTurns rest updatedPlayer enemies
        Nothing -> do
            -- Skip this enemy's turn since they're no longer alive
            processTurns rest player enemies
processTurns _ player enemies = do  -- Fallback case for unexpected inputs
    liftIO $ putStrLn "An error occurred in processing turns. Resuming combat loop."
    combatLoop player enemies


enemyTurn :: Player -> Enemy -> StateT GameState IO Player
enemyTurn player enemy = do
    let damage = max 1 (enemyAttack enemy - defense (stats player))  -- Calculate damage
    let updatedLife = max 0 (life player - damage)  -- Reduce player's life
    let updatedPlayer = player { life = updatedLife }
    liftIO $ putStrLn $ enemyName enemy ++ " attacked you for " ++ show damage ++ " damage!"
    return updatedPlayer

attackEnemy :: Player -> [Enemy] -> StateT GameState IO [Enemy]
attackEnemy player enemies = do
    liftIO $ putStrLn "Choose an enemy to attack:"
    liftIO $ mapM_ (\(i, e) -> putStrLn $ show i ++ ". " ++ enemyName e) (zip [1..] enemies)
    choice <- liftIO getLine
    case reads choice of
        [(i, "")] | i > 0 && i <= length enemies -> do
            let target = enemies !! (i - 1)
            let damage = max 1 (attack (stats player) - enemyDefense target)  -- Calculate damage
            let updatedTarget = target { enemyHealth = max 0 (enemyHealth target - damage) }
            liftIO $ putStrLn $ "\nYou dealt " ++ show damage ++ " damage to " ++ enemyName target ++ "!"

            -- Check if the enemy has been defeated
            if enemyHealth updatedTarget <= 0
                then liftIO $ putStrLn $ enemyName target ++ " defeated!"
                else return ()

            -- Update the enemies list with the modified target
            let updatedEnemies = map (\e -> if e == target then updatedTarget else e) enemies

            -- Return the enemies list, filtering out the defeated enemies
            return $ filter (\e -> enemyHealth e > 0) updatedEnemies
        _ -> do
            liftIO $ putStrLn "Invalid choice. Try again."
            attackEnemy player enemies


attemptFlee :: Player -> [Enemy] -> [(String, Int, Maybe Enemy)] -> StateT GameState IO ()
attemptFlee player enemies remainingCombatants = do
    success <- liftIO $ randomRIO (1 :: Int, 100 :: Int)  -- Generate a random Int between 1 and 100
    if success <= 30  -- 30% success rate
        then do
            liftIO $ putStrLn "You successfully fled the battle!"

            -- Sync remaining enemies with the room
            state <- get
            let currentRoom = getPlayerRoom (playerState state) (world state)
                updatedRoom = currentRoom { enemies = enemies }  -- Keep remaining enemies
                updatedWorld = map (\room -> if roomName room == roomName currentRoom then updatedRoom else room) (world state)

            put state { world = updatedWorld }
        else do
            liftIO $ putStrLn "Flee attempt failed! The battle continues."
            processTurns remainingCombatants player enemies

-- | Collect loot from all defeated enemies
collectLoot :: [Enemy] -> [Item]
collectLoot enemies = concatMap loot enemies

