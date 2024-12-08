module RoomObjectInteraction (inspectObject, findObjectByName) where

import Types
import Control.Monad.State
import Data.List (find)
import Utils.Printer(printDescription)
import Data.Char (toLower)

-- Function to inspect an object
inspectObject :: RoomObject -> StateT GameState IO ()
inspectObject obj = do
    gameState <- get
    let gameFlags = flags gameState -- Assuming flags are stored in GameState
    -- Print all descriptions whose conditions are met
    liftIO $ mapM_ (printDescription gameFlags) (descriptions obj)
    -- Enter a loop to process further actions or exit
    objectInspectLoop obj

-- Loop to handle actions or exit
objectInspectLoop :: RoomObject -> StateT GameState IO ()
objectInspectLoop obj = do
    liftIO $ putStrLn "You are inspecting the object. Type an action or 'back' to return."
    command <- liftIO getLine
    case command of
        "back" -> liftIO $ putStrLn "Returning to room view."
        _ -> case lookup command (roomActions obj) of
            Just action -> action command >> objectInspectLoop obj -- Execute action and continue loop
            Nothing -> do
                liftIO $ putStrLn "Unknown command."
                objectInspectLoop obj

-- Utility to find an object by name in the room
findObjectByName :: String -> [RoomObject] -> Maybe RoomObject
findObjectByName name = find (\obj -> map toLower name == map toLower (objectName obj)) -- Assuming RoomObject has objectName