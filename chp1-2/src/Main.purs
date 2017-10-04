module Main where

import Control.Monad.Eff.Console (logShow)
import Math (sqrt, pi)
import Prelude

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea r = pi * r * r

main = logShow (diagonal 3.0 4.0)
