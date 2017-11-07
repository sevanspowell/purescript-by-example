module Example.RandomCircle where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.DOM (addEventListener, querySelector)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Eff.Ref (REF, readRef, modifyRef, newRef)
import DOM (DOM)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, Context2D, arc, fillPath, getCanvasElementById, getCanvasWidth, getContext2D, rect, rotate, scale, setFillStyle, setStrokeStyle, strokePath, translate, withContext)
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

drawCircle :: forall eff. Context2D -> Eff (canvas :: CANVAS, random :: RANDOM | eff) Unit
drawCircle ctx = void $ do
  _ <- setFillStyle "#F78E69" ctx
  _ <- setStrokeStyle "#F1BB87" ctx

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

rotateAround :: forall eff. { x :: Number, y :: Number } -> Number -> Context2D -> Eff (canvas :: CANVAS | eff) Context2D
rotateAround pt rot ctx = do
  _ <- translate { translateX: pt.x, translateY: pt.y } ctx
  _ <- rotate rot ctx
  translate { translateX: -pt.x, translateY: -pt.y } ctx

main :: Eff ( canvas :: CANVAS
            , dom :: DOM
            , random :: RANDOM
            , console :: CONSOLE
            ) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  _ <- rotateAround { x: 300.0, y: 300.0 } (Math.pi / 4.0) ctx

  _ <- setFillStyle "#5D675B" ctx

  _ <- fillPath ctx $ do
    rect ctx
      { x: 0.0
      , y: 0.0
      , w: 600.0
      , h: 600.0
      }

  node <- querySelector "#canvas"
  for_ node $ addEventListener "click" $ void do
    log "Mouse clicked!"
    drawCircle ctx
