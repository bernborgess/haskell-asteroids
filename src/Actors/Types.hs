module Actors.Types (Ship (..)) where

import Data.Word (Word8)
import Linear (V2)

import qualified SDL

data Ship = Ship
    { shipPosition :: V2 Float
    , shipHeight :: Int
    , shipVelocity :: V2 Float
    , shipColor :: SDL.V4 Word8
    }
