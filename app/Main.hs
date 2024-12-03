module Main (main) where

import GameLoop (runGameLoop)
import InitialState (initialState)

main :: IO ()
main = runGameLoop initialState
