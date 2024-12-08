module Main (main) where

import GameLoop (runGameLoop)
import Assets.InitialState (initialState)
import Utils.Randomizer

main :: IO ()
main = do
    gameState <- randomizeGameState initialState
    runGameLoop gameState
