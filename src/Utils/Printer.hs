module Utils.Printer (module Utils.Printer) where

import qualified Data.Map as Map
import Utils.Utils
import Control.Monad (when)
import Control.Monad.State
import Data.List(intercalate)
import Types
import System.Console.ANSI

-- Print a description if its flag condition is met, using color
printDescription :: Map.Map String Bool -> (String, String, Bool) -> IO ()
printDescription flags (desc, flagKey, requiredState) =
    when (checkFlag flagKey requiredState flags) $
        putStrLn $ colorize White desc

printRoomDescription :: StateT GameState IO ()
printRoomDescription = do
    gameState <- get
    let player = playerState gameState
    let currentRoom = getPlayerRoom player (world gameState)
    let gameFlags = flags gameState

    let itemNames = map (colorize Magenta . formatItem) (items currentRoom)
    let itemLine = if null itemNames
                    then colorize White "There are no items in the room."
                    else intercalate ", " itemNames

    liftIO $ do
        printColored Cyan "\nYou are in"
        displayHeader (roomName currentRoom)
        putStrLn ""
        displaySmallHeader "Room Description"
        mapM_ (printDescription gameFlags) (description currentRoom)
        putStrLn ""
        displaySmallHeader "Items in the room"
        putStrLn itemLine

displayHeader :: String -> IO ()
displayHeader caption = do
    let lineLength = length caption + 4
        border = replicate lineLength '═'
    putStrLn $ "╔" ++ border ++ "╗"
    putStrLn $ "║  " ++ caption ++ "  ║"
    putStrLn $ "╚" ++ border ++ "╝"

-- Display a small header
displaySmallHeader :: String -> IO ()
displaySmallHeader caption = do
    putStrLn $ ">>> " ++ caption ++ " <<<"

-- Press Enter to continue prompt
pressEnterToContinue :: IO ()
pressEnterToContinue = do
    putStrLn "\nPress Enter to continue..."
    _ <- getLine
    return ()

-- Print health bars for enemies and player
printCombatHealthBars :: [Enemy] -> Player -> IO ()
printCombatHealthBars enemies player = do
    -- Print enemies section
    displayHeader "ENEMIES"
    mapM_ printEnemyHealth enemies

    -- Print player section
    displayHeader "PLAYER"
    putStrLn $ generateHealthBar (life player) (vitality (stats player)) 20

-- Print a single enemy's health with a newline after each
printEnemyHealth :: Enemy -> IO ()
printEnemyHealth enemy = do
    putStrLn $ "- " ++ enemyName enemy
    putStrLn $ generateHealthBar (enemyHealth enemy) (enemyMaxHealth enemy) 20
    putStrLn ""  -- Add a blank line after each enemy

-- Generate a single health bar
generateHealthBar :: Int -> Int -> Int -> String
generateHealthBar currentHealth maxHealth barLength =
    let filledLength = (currentHealth * barLength) `div` maxHealth
        emptyLength = barLength - filledLength
    in replicate filledLength '█' ++ replicate emptyLength '░' ++ 
       " (" ++ show currentHealth ++ "/" ++ show maxHealth ++ ")"

-- Print a string with a specified color
printColored :: Color -> String -> IO ()
printColored color text = do
    setSGR [SetColor Foreground Vivid color]
    putStrLn text
    setSGR [Reset]

-- Returns a string wrapped in color (useful for inline color)
colorize :: Color -> String -> String
colorize color text = setSGRCode [SetColor Foreground Vivid color] ++ text ++ setSGRCode [Reset]

-- Print available actions in one line with the last action in red
printAvailableActions :: [String] -> IO ()
printAvailableActions actions = putStrLn $
    colorize Cyan "Available actions: " ++
    colorize White "(" ++
    colorize White (concatMap (\action -> action ++ ", ") (init actions)) ++
    colorize Red (last actions) ++
    colorize White ")"

-- Convert healing effect to a readable string
describeHealing :: Maybe Int -> String
describeHealing Nothing = "No healing effect."
describeHealing (Just amount) = "Heals " ++ show amount ++ " HP."

-- Convert door unlocking effect to a readable string
describeUnlock :: Maybe String -> String
describeUnlock Nothing = "No unlocking effect."
describeUnlock (Just door) = "Unlocks the door: " ++ door

-- Convert Effect to a readable string
describeEffect :: Maybe Effect -> String
describeEffect Nothing = "No effects."
describeEffect (Just effect) =
    let stats = case modifyStats effect of
                  Nothing -> []
                  Just statChanges -> 
                      let formatStat name value =
                            if value > 0 then name ++ ": " ++ colorize Green ("+" ++ show value)
                            else if value < 0 then name ++ ": " ++ colorize Red (show value)
                            else ""

                          statList = filter (not . null)
                            [ formatStat "HP" (vitality statChanges)
                            , formatStat "ATK" (attack statChanges)
                            , formatStat "DEF" (defense statChanges)
                            , formatStat "SPD" (agility statChanges)
                            ]
                      in if null statList then [] else [unwords statList]

        healing = case heal effect of
                    Nothing -> []
                    Just amount -> [colorize Red $ "Restores " ++ show amount ++ " HP"]

        unlocking = case unlockDoor effect of
                      Nothing -> []
                      Just door -> [colorize Cyan $ "Unlocks: " ++ door]

        effectDescriptions = stats ++ healing ++ unlocking

    in if null effectDescriptions
       then "No effects."
       else unlines effectDescriptions