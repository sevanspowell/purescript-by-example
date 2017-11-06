module Example.Shapes where

import Data.List
import Data.Int
import Data.Traversable
import Prelude
import Data.Function (flip)

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, Context2D, arc, closePath, fillPath,
                        getCanvasElementById, getContext2D, lineTo, moveTo,
                        rect, setFillStyle, setStrokeStyle, strokePath)
import Math as Math
import Partial.Unsafe (unsafePartial)

translate
  :: forall r
   . Number
  -> Number
  -> { x :: Number, y :: Number | r }
  -> { x :: Number, y :: Number | r }
translate dx dy shape = shape
  { x = shape.x + dx
  , y = shape.y + dy
  }

type Point = { x :: Number, y :: Number }

-- Based off https://github.com/beckyconning/purescript-by-example/blob/2b2c5454b72b77fc164203fd244e96fd22b8f60f/chapter9/src/Rectangle.purs
renderPath
  :: forall eff
   . Context2D
  -> List Point
  -> Eff (canvas :: CANVAS | eff) Unit
renderPath ctx (firstPoint : points) = void $ do
  _ <- setStrokeStyle "#F19143" ctx
  strokePath ctx $ do
    _ <- moveTo ctx firstPoint.x firstPoint.y
    traverse (\pt -> lineTo ctx pt.x pt.y) points
renderPath ctx _ = pure unit
 
f :: Number -> Point
f x = { x: x * 40.0 , y: (250.0 + ((Math.sin x) * 100.0)) }

main :: Eff (canvas :: CANVAS) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  _ <- setFillStyle "#0000FF" ctx

  _ <- fillPath ctx $ rect ctx $ translate (-200.0) (-200.0)
    { x: 250.0
    , y: 250.0
    , w: 100.0
    , h: 100.0
    }

  _ <- setFillStyle "#00FF00" ctx

  _ <- fillPath ctx $ arc ctx $ translate 200.0 200.0
    { x: 300.0
    , y: 300.0
    , r: 50.0
    , start: Math.pi * 5.0 / 8.0
    , end: Math.pi * 2.0
    }

  _ <- setFillStyle "#FF0000" ctx

  -- Sector
  _ <- fillPath ctx $ do
    _ <- arc ctx $
      { x: 250.0
      , y: 250.0
      , r: 50.0
      , start: -Math.pi / 2.0
      , end: 0.0
      }
    _ <- moveTo ctx 250.0 250.0
    _ <- lineTo ctx 250.0 200.0
    _ <- lineTo ctx 300.0 250.0
    closePath ctx

  _ <- fillPath ctx $ do
    _ <- moveTo ctx 300.0 260.0
    _ <- lineTo ctx 260.0 340.0
    _ <- lineTo ctx 340.0 340.0
    closePath ctx

  renderPath ctx $ f <$> flip div 10.0 <$> toNumber <$> 0 .. 1000
