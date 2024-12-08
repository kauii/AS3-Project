module Utils.Printer (printDescription, printRoomDescription) where

import qualified Data.Map as Map
import Utils.Utils
import Control.Monad (when)
import Control.Monad.State
import Types
import Data.List ( intercalate )
import System.Console.ANSI

-- Print a description if its flag condition is met
printDescription :: Map.Map String Bool -> (String, String, Bool) -> IO ()
printDescription flags (desc, flagKey, requiredState) =
    when (checkFlag flagKey requiredState flags) $
        putStrLn desc

printRoomDescription :: StateT GameState IO ()
printRoomDescription = do
    gameState <- get
    let player = playerState gameState
    let currentRoom = getPlayerRoom player (world gameState)
    let gameFlags = flags gameState

    let itemNames = map formatItem (items currentRoom)
    let itemLine = if null itemNames
                    then colorize White "There are no items in the room."
                    else "The following items are in the room: " ++ colorize Magenta (intercalate ", " itemNames)

    liftIO $ do
        putStrLn $ "You are in: " ++ colorize Cyan (roomName currentRoom)
        mapM_ (printDescription gameFlags) (description currentRoom)
        putStrLn itemLine      