module Example.Random where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, random)
import Data.Array ((..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (Context2D, CANVAS, strokePath, fillPath, arc, setStrokeStyle, setFillStyle, getContext2D, getCanvasElementById)
import Math as Math
import Partial.Unsafe (unsafePartial)

strokeAndFillPath
  :: forall eff a
   . Context2D
  -> Eff (canvas :: CANVAS | eff) a 
  -> Eff (canvas :: CANVAS | eff) a
strokeAndFillPath ctx path = do
  _ <- fillPath ctx path
  strokePath ctx path

main :: Eff (canvas :: CANVAS, random :: RANDOM) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  _ <- setFillStyle "#FF0000" ctx
  _ <- setStrokeStyle "#000000" ctx

  for_ (1 .. 100) \_ -> do
    x <- random
    y <- random
    r <- random

    let path = arc ctx
         { x     : x * 600.0
         , y     : y * 600.0
         , r     : r * 50.0
         , start : 0.0
         , end   : Math.pi * 2.0
         }

    strokeAndFillPath ctx path
