{-# LANGUAGE NamedFieldPuns #-}

module Actors.Ship (
    newShip,
    initializeShip,
    shipSetPosition,
)
where

import Control.Monad.State (gets, modify, when)
import Data.Ord (clamp)
import GHC.Float (int2Float)
import Linear (Epsilon, V2 (V2), V3 (..), zero, (^*), (^/))
import Linear.Matrix (M33, fromQuaternion, (!*))
import Linear.Metric (norm)
import Linear.Quaternion (axisAngle)

import qualified SDL

import Actors.Types (Ship (..))
import Components.DrawComponent (drawPolygon)
import Game.State (DrawProcedure, GameProcedure, GameState (..), ProcessInputProcedure, UpdateProcedure, addActor, addDrawable)

newShip :: Ship
newShip =
    Ship
        { shipPosition = zero
        , shipHeight = 20
        , shipVelocity = zero
        , -- , shipColor = SDL.V4 148 0 211 255
          shipColor = SDL.V4 255 255 255 255
        , shipForwardForce = 500.0
        , shipRotationForce = 5.0
        , -- Rigid body
          shipMass = 1.0
        , shipAngularSpeed = 0.0
        , shipAcceleration = 0.0
        , shipRotation = 0.0
        }

initializeShip :: GameProcedure
initializeShip = do
    addDrawable shipDraw
    addActor (Just shipProcessInput) shipUpdate

shipSetPosition :: V2 Float -> GameProcedure
shipSetPosition pos = do
    ship <- gets gameShip
    modify $ \gs -> gs{gameShip = ship{shipPosition = pos}}

shipProcessInput :: ProcessInputProcedure
shipProcessInput ks = do
    Ship{shipForwardForce, shipRotation = rot, shipRotationForce} <- gets gameShip
    let thrust = ks SDL.ScancodeW || ks SDL.ScancodeUp
        left = ks SDL.ScancodeA || ks SDL.ScancodeLeft
        right = ks SDL.ScancodeD || ks SDL.ScancodeRight
        front = V2 (cos rot) (sin rot) -- GetForward
    when thrust $ applyForce $ front ^* shipForwardForce

    let newAngularSpeed | left && right = 0 | left = -shipRotationForce | right = shipRotationForce | otherwise = 0
    modify $ \gs -> gs{gameShip = (gameShip gs){shipAngularSpeed = newAngularSpeed}}

shipUpdate :: UpdateProcedure
shipUpdate _ deltaTime = do
    -- TODO: Move to RigidBodyComponent
    Ship{shipPosition, shipVelocity, shipAcceleration, shipRotation, shipAngularSpeed} <- gets gameShip
    let newVelocity = shipVelocity + shipAcceleration ^* deltaTime
        newVelocity' = if clamp (-0.01, 0.01) (norm newVelocity) == norm newVelocity then zero else newVelocity
        newPosition = shipPosition + newVelocity' ^* deltaTime
        newRotation = shipRotation + shipAngularSpeed * deltaTime

    -- TODO: Screen Wrap

    modify $ \gs -> gs{gameShip = (gameShip gs){shipVelocity = newVelocity', shipPosition = newPosition, shipAcceleration = zero, shipRotation = newRotation}}

shipDraw :: DrawProcedure
shipDraw renderer = do
    Ship{shipPosition = V2 posX posY, shipHeight, shipColor, shipRotation = theta} <- gets gameShip

    -- Get rotation matrix
    let m = createRotation theta
        h = int2Float shipHeight
        triangle =
            [ V2 (-h) (h / 2.0)
            , V2 h 0
            , V2 (-h) ((-h) / 2.0)
            ]

        f :: V2 Float -> V2 Float
        f v =
            let (V2 x y) = transform2 m v
             in V2 (x + posX) (y + posY)

    drawPolygon (f <$> triangle) (Just shipColor) renderer

applyForce :: V2 Float -> GameProcedure
applyForce force = do
    -- ? We need lenses to get the right actor...
    Ship{shipMass, shipAcceleration} <- gets gameShip
    modify $ \gs -> gs{gameShip = (gameShip gs){shipAcceleration = shipAcceleration + force ^/ shipMass}}

createRotation :: (Epsilon a, Floating a) => a -> M33 a
createRotation theta =
    fromQuaternion (axisAngle (V3 0 0 1) theta)

-- | Apply a 3×3 “homogeneous” matrix to a 2D vector (with implicit w=1).
transform2 :: (Num a) => M33 a -> V2 a -> V2 a
transform2 m (V2 x y) =
    let V3 x' y' _w' = m !* V3 x y 1
     in V2 x' y'