module Main (main) where

import GameLoop (runGameLoop)
import Assets.InitialState (initialState)
import Utils.Randomizer
import Utils.Printer
import System.Console.ANSI (Color(Green))

main :: IO ()
main = do
    displayHeader "Welcome to the Text Adventure 'The Dark Overlord's Castle'"
    putStrLn ""
    printColored Green "The iron doors groan as you shove them open, revealing a grand hall cloaked in shadow.\nColumns adorned with grotesque carvings rise to a vaulted ceiling, their eyes seeming to follow you.\nThe air is cold and heavy, carrying the faint stench of decay."
    printColored Green "Somewhere in this castle the Dark Overlord awaits, his presence pressing against your resolve like a storm cloud.\nThe world's fate hinges on what happens next."
    putStrLn ""
    putStrLn "Hint: You can show the room when you write 'inspect room'"
    gameState <- randomizeGameState initialState
    runGameLoop gameState
