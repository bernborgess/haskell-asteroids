module Main where

import Control.Monad.State (evalStateT)
import Foreign.C.Types (CInt)

import Game (
    initialGameState,
    initialize,
    runLoop,
    shutdown,
 )

screenWidth, screenHeight :: CInt
screenWidth = 1280
screenHeight = 720

main :: IO ()
main = flip evalStateT initialGameState $ do
    gameData <- initialize screenWidth screenHeight
    runLoop gameData
    shutdown