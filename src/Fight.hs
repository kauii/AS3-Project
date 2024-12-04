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
            liftIO $ displayHeader "Combat"  -- Add a combat header
            liftIO $ putStrLn "Enemies spotted!"
            liftIO $ mapM_ (\enemy -> putStrLn $ "- " ++ enemyName enemy) enemiesInRoom  -- List enemies
            combatLoop player enemiesInRoom

-- | Combat loop: Runs until combat ends
combatLoop :: Player -> [Enemy] -> StateT GameState IO ()
combatLoop player enemies = do
    if null enemies
        then do
            liftIO $ putStrLn "All enemies defeated!"
            notImplemented "Drop loot into the room."
        else if life player <= 0
            then liftIO $ putStrLn "You have been defeated..."
            else do
                let combatants = determineTurnOrder player enemies
                processTurns combatants player enemies

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
    liftIO $ putStrLn $ name ++ "'s turn (Player)"
    liftIO $ putStrLn "Actions available: (Attack, Use Item, Flee)"
    action <- liftIO getLine
    let parsedAction = parseActionFight action
    case parsedAction of
        Just (Attack _) -> notImplemented "Player attacks an enemy."
        Just (UseItem _) -> notImplemented "Player uses an item."
        Just Flee -> notImplemented "Player attempts to flee."
        _ -> do
            liftIO $ putStrLn "Invalid action. Try again."
            processTurns ((name, agility (stats player), Nothing) : rest) player enemies
processTurns ((name, _, Just enemy) : rest) player enemies = do
    liftIO $ putStrLn $ name ++ "'s turn (Enemy)"
    notImplemented "Enemy attacks the player."
    processTurns rest player enemies

-- | Placeholder function to handle unimplemented features
notImplemented :: String -> StateT GameState IO ()
notImplemented message = liftIO $ putStrLn $ "Feature not implemented yet: " ++ message
