module Fight (enterCombat) where

import Data.List (sortOn)
import Types
import Utils
import Control.Monad.State
import System.Random (randomRIO)

-- | Enter combat when the room contains enemies
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

-- | Combat loop: Runs until combat ends
combatLoop :: Player -> [Enemy] -> StateT GameState IO ()
combatLoop player enemies = do
    if null enemies
        then do
            liftIO $ putStrLn "All enemies defeated!"
            updatePlayerState player -- Update player's state after combat
            notImplemented "Drop loot into the room."
            lift pressEnterToContinue
        else if life player <= 0
            then do
                liftIO $ putStrLn "You have been defeated..."
                lift pressEnterToContinue
                updatePlayerState player -- Update player's state to reflect death
            else do
                lift pressEnterToContinue
                -- Print health bars for both player and enemies
                liftIO $ printCombatHealthBars enemies player
                -- Determine turn order based on agility
                let combatants = determineTurnOrder player enemies
                -- Process turns
                processTurns combatants player enemies

-- | Update the player's state in the game state
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

-- | Process turns
processTurns :: [(String, Int, Maybe Enemy)] -> Player -> [Enemy] -> StateT GameState IO ()
processTurns [] player enemies = combatLoop player enemies
processTurns ((name, _, Nothing) : rest) player enemies = do
    liftIO $ putStrLn $ "\n" ++ name ++ "'s turn (Player)"
    liftIO $ putStrLn "Actions available: (Attack, UseItem, Flee)"
    action <- liftIO getLine
    let parsedAction = parseActionFight action
    case parsedAction of
        Just (Attack _) -> do
            updatedEnemies <- attackEnemy player enemies
            processTurns rest player updatedEnemies
        Just (UseItem _) -> notImplemented "Player uses an item."
        Just Flee -> notImplemented "Player attempts to flee."
        _ -> do
            liftIO $ putStrLn "Invalid action. Try again."
            processTurns ((name, agility (stats player), Nothing) : rest) player enemies
processTurns ((name, _, Just enemy) : rest) player enemies = do
    liftIO $ putStrLn $ "\n" ++ name ++ "'s turn (Enemy)"
    updatedPlayer <- enemyTurn player enemy
    updatePlayerState updatedPlayer -- Update player's health after the enemy's turn
    processTurns rest updatedPlayer enemies

-- | Placeholder function to handle unimplemented features
notImplemented :: String -> StateT GameState IO ()
notImplemented message = liftIO $ putStrLn $ "Feature not implemented yet: " ++ message

-- | Enemy performs an attack on the player
enemyTurn :: Player -> Enemy -> StateT GameState IO Player
enemyTurn player enemy = do
    let damage = max 1 (enemyAttack enemy - defense (stats player))  -- Calculate damage
    let updatedLife = max 0 (life player - damage)  -- Reduce player's life
    let updatedPlayer = player { life = updatedLife }
    liftIO $ putStrLn $ enemyName enemy ++ " attacked you for " ++ show damage ++ " damage!"
    return updatedPlayer

-- | Player attacks an enemy
attackEnemy :: Player -> [Enemy] -> StateT GameState IO [Enemy]
attackEnemy player enemies = do
    liftIO $ putStrLn "Choose an enemy to attack:"
    liftIO $ mapM_ (\(i, e) -> putStrLn $ show i ++ ". " ++ enemyName e) (zip [1..] enemies)
    choice <- liftIO getLine
    case reads choice of
        [(i, "")] | i > 0 && i <= length enemies -> do
            let target = enemies !! (i - 1)
            let damage = max 1 (attack (stats player) - enemyDefense target)  -- Calculate damage
            let updatedEnemies = map (\e -> if e == target then e { enemyHealth = max 0 (enemyHealth e - damage) } else e) enemies
            liftIO $ putStrLn $ "You dealt " ++ show damage ++ " damage to " ++ enemyName target ++ "!"
            return $ filter (\e -> enemyHealth e > 0) updatedEnemies  -- Remove dead enemies
        _ -> do
            liftIO $ putStrLn "Invalid choice. Try again."
            attackEnemy player enemies

-- | Print health bars for enemies and player
printCombatHealthBars :: [Enemy] -> Player -> IO ()
printCombatHealthBars enemies player = do
    -- Print enemies section
    displayHeader "ENEMIES"
    mapM_ printEnemyHealth enemies

    -- Print player section
    displayHeader "PLAYER"
    putStrLn $ generateHealthBar (life player) (vitality (stats player)) 20

-- | Print a single enemy's health with a newline after each
printEnemyHealth :: Enemy -> IO ()
printEnemyHealth enemy = do
    putStrLn $ "- " ++ enemyName enemy
    putStrLn $ generateHealthBar (enemyHealth enemy) (enemyMaxHealth enemy) 20
    putStrLn ""  -- Add a blank line after each enemy

-- | Generate a single health bar
generateHealthBar :: Int -> Int -> Int -> String
generateHealthBar currentHealth maxHealth barLength =
    let filledLength = (currentHealth * barLength) `div` maxHealth
        emptyLength = barLength - filledLength
        percentage = (currentHealth * 100) `div` maxHealth
    in replicate filledLength '█' ++ replicate emptyLength '░' ++ " (" ++ show percentage ++ "%)"

