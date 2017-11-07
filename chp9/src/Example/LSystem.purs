module Example.LSystem where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Array (concatMap, foldM)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, closePath, fillPath, getCanvasElementById,
                        getContext2D, lineTo, moveTo, setFillStyle,
                        setShadowBlur, setShadowColor, setShadowOffsetX,
                        setShadowOffsetY, setStrokeStyle, strokePath)
import Math as Math
import Partial.Unsafe (unsafePartial)

lsystem :: forall a m s. Monad m =>
                         Array a ->
                         (a -> Array a) ->
                         (s -> a -> m s) ->
                         Int ->
                         s -> m s
lsystem init prod interpret n state = executeSentence $ buildSentence init n
  where
  buildSentence s 0 = s
  buildSentence s m = buildSentence (concatMap prod s) (m - 1)

  executeSentence = foldM interpret state

type Angle = Number

data Alphabet = L Angle | R Angle | F

type Sentence = Array Alphabet

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

main :: Eff (canvas :: CANVAS) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  let
    l :: Alphabet
    l = L $ Math.pi / 3.0

    r :: Alphabet
    r = R $ Math.pi / 3.0

    r2 :: Alphabet
    r2 = R $ Math.pi / 6.0

    initial :: Sentence
    initial = [F, r2, r, F, r, r, F, r, r]

    productions :: Alphabet -> Sentence
    productions (L a) = [L a]
    productions (R a) = [R a]
    productions F = [F, l, F, r, r, F, l, F]

    interpret :: State -> Alphabet -> Eff (canvas :: CANVAS) State
    interpret state (L a) = pure $ state { theta = state.theta - a }
    interpret state (R a) = pure $ state { theta = state.theta + a }
    interpret state F = do
      let x = state.x + Math.cos state.theta * 1.5
          y = state.y + Math.sin state.theta * 1.5
      _ <- lineTo ctx x y
      pure { x, y, theta: state.theta }

    initialState :: State
    initialState = { x: 120.0, y: 200.0, theta: 0.0 }

  _ <- moveTo ctx initialState.x initialState.y

  _ <- setFillStyle "#ABE188" ctx
  _ <- setShadowOffsetX 10.0 ctx
  _ <- setShadowOffsetY 10.0 ctx
  _ <- setShadowBlur 3.0 ctx
  _ <- setShadowColor "#777" ctx

  fillPath ctx $ do 
    _ <- lsystem initial productions interpret 5 initialState
    closePath ctx
