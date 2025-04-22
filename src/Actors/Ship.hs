{-# LANGUAGE NamedFieldPuns #-}

module Actors.Ship (
    newShip,
    initializeShip,
    shipSetPosition,
)
where

import Control.Monad.State (gets, modify)
import Linear (V2 (V2), zero)

import qualified SDL

import Actors.Types (Ship (..))

import Components.DrawComponent (drawPolygon)
import GHC.Float (int2Float)
import Game.State (DrawProcedure, GameProcedure, GameState (..), UpdateProcedure, addActor, addDrawable)

newShip :: Ship
newShip =
    Ship
        { shipPosition = zero
        , shipHeight = 20
        , shipVelocity = zero
        , shipColor = SDL.V4 148 0 211 255
        }

initializeShip :: GameProcedure
initializeShip = do
    addDrawable shipDraw
    addActor Nothing shipUpdate

shipSetPosition :: V2 Float -> GameProcedure
shipSetPosition pos = do
    ship <- gets gameShip
    modify $ \gs -> gs{gameShip = ship{shipPosition = pos}}

shipUpdate :: UpdateProcedure
shipUpdate _ deltaTime = do
    pure ()

shipDraw :: DrawProcedure
shipDraw renderer = do
    Ship{shipPosition = V2 posX posY, shipHeight, shipColor} <- gets gameShip
    let h = int2Float shipHeight
        triangle = [(-h, h / 2.0), (h, 0), (-h, (-h) / 2.0)]
        vertices = map (\(x, y) -> (x + posX, y + posY)) triangle

    drawPolygon vertices (Just shipColor) renderer
