module Main where

import GameLoop (runGameLoop) -- Replace `Game` with the actual module name where these are defined.
import InitialState (initialState)

main :: IO ()
main = runGameLoop initialState