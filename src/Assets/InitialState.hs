module Assets.InitialState (initialState) where

import Types
import Control.Monad.State
import qualified Data.Map as Map
import Utils.Utils
import Types (ItemType(KeyItem))
import Assets.Rooms

-- Initial game state
initialState :: GameState
initialState = GameState
    { playerState = initialPlayer
    , world = [startingRoom, hallway, armory]
    , flags = Map.fromList [("statue_searched", False), ("compartment_opened", False), ("default_true", True)]
    }

object1 :: RoomObject
object1 = RoomObject {
    objectName = "Mysterious Statue",
    descriptions = [
        ("An ancient statue covered in moss.", "is_true", True),
        ("The statue has a hidden compartment.", "statue_searched", True),
        ("The compartment is open and empty", "compartment_opened", True)
    ],
    roomObjectItems = [],
    roomActions = [
        ("search", \_ -> do
            modify (\gs -> gs { flags = Map.insert "statue_searched" True (flags gs) })
            liftIO $ putStrLn "You discover a hidden compartment in the statue!"),
        ("open", \_ -> do
            modify (\gs -> gs { flags = Map.insert "compartment_opened" True (flags gs) })
            liftIO $ putStrLn "You discover a hidden compartment in the statue!")
    ]
}

-- Initial player state
initialPlayer :: Player
initialPlayer = Player
    { location = "Starting Room"
    , inventory = []
    , life = 90
    , stats = PlayerStats { vitality = 100, attack = 20, defense = 5, agility = 20 }
    , quests = []
    , weapon = Nothing
    , armor = Nothing
    }
