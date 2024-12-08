module Assets.ProgressRelevant.RoomObjects (module Assets.ProgressRelevant.RoomObjects) where

import Types
import Utils.Utils
import Assets.ProgressRelevant.Items
import Control.Monad.State

sign :: RoomObject
sign = RoomObject {
    objectName = "Sign",
    descriptions = [("An old sign. It says: Beware the Vault of Eternal Rest. All who enter must tread carefully.", "default_true", True)],
    roomObjectItems = [],
    roomActions = []
}

chandelier :: RoomObject
chandelier = RoomObject {
    objectName = "Chandelier",
    descriptions = [("A low hanging chandelier. You notice that a candle is missing.", "chandelier_fixed", False), ("A low hanging chandelier. All candles are burning brightly", "chandelier_fixed", True)],
    roomObjectItems = [],
    roomActions = [
        ("use candle", \_ -> do
            hasCandle <- checkIfInInventory "Candle" 1
            if hasCandle
                then do
                    removeItemFromPlayer "Candle" 1
                    setGameFlag "chandelier_fixed" True
                    addItemToPlayer rustedKey
                    liftIO $ putStrLn "You light the candle and put it in the empty candlestick. You hear a click and a Rusted Key drops to the floor. You pick it up."
                else do
                    gameState <- get
                    if checkFlag "chandelier_fixed" True (flags gameState)
                        then do
                            liftIO $ putStrLn "All candles are already burning."
                        else do
                            liftIO $ putStrLn "You don't have a candle."
        )
    ]
}

bookshelve :: RoomObject
bookshelve = RoomObject {
    objectName = "Bookshelve",
    descriptions = [("The shelve is almost empty. There is only one dusty book left.", "default_true", True)],
    roomObjectItems = [],
    roomActions = [
        ("read book", \_ -> do
            liftIO $ putStrLn "You don't undestand the language the book is written in. Someone scribbled in the book: To open the way, align the elements. Only then can you speak of eternal rest.")
    ]
}

chest :: RoomObject
chest = RoomObject {
    objectName = "Chest",
    descriptions = [("The chest is locked.", "chest_unlocked", False), ("The chest is unlocked. There is a scroll inside.", "chest_unlocked", True)],
    roomObjectItems = [],
    roomActions = [
        ("open chest", \_ -> do
            gameState <- get
            if checkFlag "chest_unlocked" False (flags gameState)
                then do
                    hasKey <- checkIfInInventory "Silver Key" 1
                    if hasKey
                        then do
                            setGameFlag "chest_unlocked" True
                            liftIO $ putStrLn "You unlock the chest with your silver key and open it. There is a scroll inside."
                        else do
                            liftIO $ putStrLn "You cannot unlock the chest without the correct key"
                else do
                    liftIO $ putStrLn "The chest is already open."),
        ("read scroll", \_ -> do
            gameState <- get
            if checkFlag "chest_unlocked" False (flags gameState)
                then do
                    liftIO $ putStrLn "The chest is unlocked. What scroll are you talking about?"
                else do
                    liftIO $ putStrLn "Someone wrote on the scroll: To open the way, light the fire, calm the water, and steady the earth.")
    ]
}

portrait :: RoomObject
portrait = RoomObject {
    objectName = "Portrait",
    descriptions = [("A dusty portrait of a nobleman.", "default_true", True)],
    roomObjectItems = [],
    roomActions = [
        ("move portrait", \_ -> do
            gameState <- get
            if checkFlag "taken_dinner_key" False (flags gameState)
                then do
                    setGameFlag "taken_dinner_key" True
                    addItemToPlayer smallKey
                    liftIO $ putStrLn "A small key is hidden in a small crevice behind the portrait. You take it."
                else do
                    liftIO $ putStrLn "There's nothing behind the portrait.")
    ]
}

cabinet :: RoomObject
cabinet = RoomObject {
    objectName = "Cabinet",
    descriptions = [("An old cabinet. It is locked", "cabinet_opened", False),
                    ("An old cabinet. It's unlocked and empty.", "cabinet_opened", False)],
    roomObjectItems = [],
    roomActions = [
        ("open cabinet", \_ -> do
            gameState <- get
            if checkFlag "cabinet_opened" False (flags gameState)
                then do
                    setGameFlag "cabinet_opened" True
                    addItemToPlayer silverKey
                    liftIO $ putStrLn "You open the cabinet and find a silver key in it. You take it."
                else do
                    liftIO $ putStrLn "The cabinet is already open.")
    ]
}

anvil :: RoomObject
anvil = RoomObject {
    objectName = "Anvil",
    descriptions = [("The runes seem to hypnotize you. The longer you stare at them, the better you seem to understand them. They say: Move the former owner to the side!", "default_true", True)],
    roomObjectItems = [],
    roomActions = []
}

switches :: RoomObject
switches = RoomObject
    { objectName = "Switches"
    , descriptions =
        [ ("Three ancient switches are embedded in the wall. The symbols for Fire, Water, and Earth are carved above them. (To flip a lever, write 'flip element', to speak the passphrase, write 'speak passphrase')", "default_true", True)
        ]
    , roomObjectItems = []
    , roomActions =
        [ ("flip fire", \_ -> flipSwitch "Fire")
        , ("flip water", \_ -> flipSwitch "Water")
        , ("flip earth", \_ -> flipSwitch "Earth")
        , ("speak passphrase", const speakPassphrase)
        ]
    }