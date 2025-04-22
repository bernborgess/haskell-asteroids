module Actors.Types (Ship (..)) where

import Data.Word (Word8)
import Linear (V2)

import qualified SDL

data Ship = Ship
    { shipPosition :: V2 Float
    , shipHeight :: Int
    , shipColor :: SDL.V4 Word8
    , shipForwardForce :: Float
    , shipRotationForce :: Float
    , -- Rigid Body
      shipMass :: Float
    , shipAngularSpeed :: Float
    , shipVelocity :: V2 Float
    , shipAcceleration :: V2 Float
    , shipRotation :: Float
    }
