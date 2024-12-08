module Assets.ProgressRelevant.Boss (enterBossBattle) where

import Types
import Utils.Utils
import Utils.Printer
import Inventory
import Control.Monad.State
import Control.Monad(when)
import Data.List(sortOn)


-- | Define the final boss
finalBoss :: Enemy
finalBoss = Enemy
    { enemyName = "Dark Overlord"
    , enemyHealth = 100
    , enemyMaxHealth = 100
    , enemyAttack = 20
    , enemyDefense = 15
    , enemyAgility = 10
    , loot = []  -- No loot, this is the final boss
    }

-- | Enter the boss battle
enterBossBattle :: StateT GameState IO ()
enterBossBattle = do
    liftIO $ displayHeader "FINAL BOSS BATTLE"
    liftIO $ putStrLn "The Dark Overlord stands before you, radiating pure evil.\n"
    state <- get
    let player = playerState state
    bossFightLoop player finalBoss

-- | Boss fight loop
bossFightLoop :: Player -> Enemy -> StateT GameState IO ()
bossFightLoop player boss = do
    if life player <= 0
        then do
            liftIO $ putStrLn "You have been defeated by the Dark Overlord..."
            lift pressEnterToContinue
        else if enemyHealth boss <= 0
            then do
                liftIO $ putStrLn "You have defeated the Dark Overlord! Peace returns to the land."
                lift pressEnterToContinue
                displayEnding
        else do
            lift pressEnterToContinue
            liftIO $ printBossBattleStatus player boss
            let combatants = determineBossTurnOrder player boss
            processBossTurns combatants player boss

-- | Print player and boss health bars
printBossBattleStatus :: Player -> Enemy -> IO ()
printBossBattleStatus player boss = do
    displayHeader "BOSS STATUS"
    putStrLn $ "- " ++ enemyName boss
    putStrLn $ generateHealthBar (enemyHealth boss) (enemyMaxHealth boss) 20
    displayHeader "PLAYER STATUS"
    putStrLn $ generateHealthBar (life player) (vitality (stats player)) 20

-- | Determine turn order between player and boss
determineBossTurnOrder :: Player -> Enemy -> [(String, Int, Maybe Enemy)]
determineBossTurnOrder player boss =
    let playerAgility = agility (stats player)
        bossAgility = enemyAgility boss
    in reverse $ sortOn (\(_, ag, _) -> ag) [("Player", playerAgility, Nothing), (enemyName boss, bossAgility, Just boss)]

-- | Process player and boss turns
processBossTurns :: [(String, Int, Maybe Enemy)] -> Player -> Enemy -> StateT GameState IO ()
processBossTurns [] player boss = bossFightLoop player boss
processBossTurns ((name, _, Nothing) : rest) player boss = do
    liftIO $ putStrLn $ "\n" ++ name ++ "'s turn (Player)"
    liftIO $ putStrLn "Actions available: (Attack, OpenInv)"
    action <- liftIO getLine
    let parsedAction = parseActionFight action
    case parsedAction of
        Just Attack -> do
            updatedBoss <- playerAttackBoss player boss
            processBossTurns rest player updatedBoss
        Just OpenInv -> do
            updatedPlayer <- openInventory True
            processBossTurns rest updatedPlayer boss
        _ -> do
            liftIO $ putStrLn "Invalid action. Try again."
            processBossTurns ((name, agility (stats player), Nothing) : rest) player boss
processBossTurns ((name, _, Just enemyBoss) : rest) player boss = do
    liftIO $ putStrLn $ "\n" ++ name ++ "'s turn (Boss)"
    updatedPlayer <- bossAttackPlayer player enemyBoss
    processBossTurns rest updatedPlayer boss



-- | Player attacks the boss
playerAttackBoss :: Player -> Enemy -> StateT GameState IO Enemy
playerAttackBoss player boss = do
    let damage = max 1 (attack (stats player) - enemyDefense boss)
    let updatedBoss = boss { enemyHealth = max 0 (enemyHealth boss - damage) }
    liftIO $ putStrLn $ "You dealt " ++ show damage ++ " damage to " ++ enemyName boss ++ "!"
    when (enemyHealth updatedBoss <= 0) $ liftIO $ putStrLn $ enemyName boss ++ " defeated!"
    return updatedBoss

-- | Boss attacks the player
bossAttackPlayer :: Player -> Enemy -> StateT GameState IO Player
bossAttackPlayer player boss = do
    let damage = max 1 (enemyAttack boss - defense (stats player))
    let updatedPlayer = player { life = max 0 (life player - damage) }
    liftIO $ putStrLn $ enemyName boss ++ " attacked you for " ++ show damage ++ " damage!"
    return updatedPlayer

-- | Display the game ending
displayEnding :: StateT GameState IO ()
displayEnding = liftIO $ do
    displayHeader "CONGRATULATIONS!"
    putStrLn "You have vanquished the Dark Overlord and saved the kingdom!"
    putStrLn "Thank you for playing!"
