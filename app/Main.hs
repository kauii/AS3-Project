module Main (main) where

import GameLoop (runGameLoop)
import Assets.InitialState (initialState)

main :: IO ()
main = runGameLoop initialState
