module Types (
    Direction(..),
    Room(..),
    RoomObject(..),
    Item(..),
    Effect(..),
    Player(..),
    PlayerStats(..),
    Quest(..),
    Enemy(..),
    Door(..),
    GameState(..),
    Action(..)
) where

import Control.Monad.State
import qualified Data.Map as Map

-- Directions for navigation
data Direction = North | South | East | West deriving (Show, Eq)

-- Rooms, which can contain items, enemies, NPCs, and doors
data Room = Room {
    roomName :: String,            -- Name of the room
    description :: String,         -- Description shown to the player
    roomObjects :: [RoomObject],
    exits :: [(Direction, String)], -- Possible exits (Direction, connected room name)
    items :: [Item],               -- Items in the room
    enemies :: [Enemy],            -- Enemies present in the room
    doors :: [Door]                -- Doors connected to this room
} deriving (Show)

data RoomObject = RoomObject {
    objectName :: String,
    descriptions :: [(String, String, Bool)],
    roomObjectItems :: [Item],
    roomActions :: [(String, String -> StateT GameState IO ())]
}

-- Items, which can have effects when used
data Item = Item {
    itemName :: String,            -- Name of the item
    itemDescription :: String,     -- Description of the item
    effect :: Maybe Effect         -- Effect the item has when used (if any)
} deriving (Show)

-- Effects of items, which can modify player stats or state
data Effect = Effect {
    modifyStats :: Maybe PlayerStats, -- Changes to player stats (e.g., +ATK, +DEF)
    heal :: Maybe Int,                -- Healing amount for the player
    unlockDoor :: Maybe String        -- Unlock a door with this name
} deriving (Show)

-- Player stats, such as attack and defense
data PlayerStats = PlayerStats {
    vitality :: Int,
    attack :: Int,                 -- Player's attack power
    defense :: Int,               -- Player's defense power
    agility :: Int
} deriving (Show, Eq)

-- Player state, tracking the player's progress
data Player = Player {
    location :: String,            -- Current room name
    inventory :: [Item],           -- Items the player carries
    life :: Int,                   -- Player's current life
    stats :: PlayerStats,          -- The player's stats
    quests :: [Quest]              -- List of quests the player is working on
} deriving (Show)

-- Quests for objectives
data Quest = Quest {
    questName :: String,           -- Name of the quest
    questDescription :: String,    -- Details of the quest
    requirements :: [Item],        -- Items or conditions required to complete the quest
    isComplete :: Bool             -- Whether the quest is completed
} deriving (Show)

-- Enemies in the game, which can be placed in rooms
data Enemy = Enemy {
    enemyName :: String,           -- Name of the enemy
    enemyHealth :: Int,                 -- Enemy's health
    enemyAttack :: Int,            -- Enemy's attack power
    enemyDefense :: Int,                -- Enemy's defense
    loot :: [Item]                 -- Items dropped upon defeat
} deriving (Show)

-- Doors connecting rooms
data Door = Door {
    doorName :: String,            -- Name of the door
    leadsTo :: String,             -- Room the door connects to
    isLocked :: Bool,              -- Whether the door is locked
    keyRequired :: Maybe String    -- Name of the key required to unlock
} deriving (Show)

-- Game state encapsulating player and world
data GameState = GameState {
    playerState :: Player,    -- Current state of the player
    world :: [Room],                -- List of all rooms in the game world
    flags :: Map.Map String Bool
} deriving (Show)

-- Actions a player can perform
data Action = Go Direction         -- Move in a specific direction
            | Take String          -- Pick up an item by name
            | Drop String          -- Drop an item by name
            | Inspect String       -- Inspect an item, NPC, or environment
            | Attack String        -- Attack an enemy by name
            | TalkTo String        -- Talk to an NPC by name
            | OpenDoor String      -- Open a door by name
            | UseItem String       -- Use an item by name
            | OpenInv              -- Opens the player's inventory
            | Quit                 -- Quit the game
            | Back                 -- Go Back
            deriving (Show, Eq)


-- Helper Show Functions (Maybe remove?)
instance Show RoomObject where
    show (RoomObject name descs items actions) =
        "RoomObject { " ++
        "descriptions = " ++ show descs ++ ", " ++
        "roomObjectItems = " ++ show items ++ ", " ++
        "roomActions = <functions> }"