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
import Control.Monad.Eff.Console (CONSOLE, log)

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

data Alphabet = L | R | F Boolean

type Sentence = Array Alphabet

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

main = void $ unsafePartial do
  -- Just canvas <- getCanvasElementById "canvas"
  -- ctx <- getContext2D canvas

  let
    initial :: Sentence
    initial = [(F true)]

    productions :: Alphabet -> Sentence
    productions L = [L]
    productions R = [R]
    productions (F false) = [(F false), L, (F true), L, (F false), R, (F true), R, (F false), R, (F true), R, (F false), L, (F true), L, (F false)]
    productions (F true) = [(F true), R, (F false), R, (F true), L, (F false), L, (F true), L, (F false), L, (F true), R, (F false), R, (F true)]

    -- interpret :: State -> Alphabet -> Eff (canvas :: CANVAS) State
    -- interpret state L     = pure $ state { theta = state.theta - Math.pi / 3.0 }
    -- interpret state R     = pure $ state { theta = state.theta + Math.pi / 3.0 }
    -- interpret state (F _) = do
    --   let x = state.x + Math.cos state.theta * 1.5
    --       y = state.y + Math.sin state.theta * 1.5
    --   _ <- lineTo ctx x y
    --   pure { x, y, theta: state.theta }

    interpretTrace :: State -> Alphabet -> Eff (console :: CONSOLE) State 
    interpretTrace state L         = do
                                     _ <- log $ show "L"
                                     pure $ state
    interpretTrace state R         = do
                                     _ <- log $ show "R"
                                     pure $ state
    interpretTrace state (F true)  = do
                                     _ <- log $ "(F t)"
                                     pure $ state
    interpretTrace state (F false) = do
                                     _ <- log $ "(F f)"
                                     pure $ state

    initialState :: State
    initialState = { x: 120.0, y: 200.0, theta: 0.0 }

  -- _ <- moveTo ctx initialState.x initialState.y

  -- _ <- setFillStyle "#ABE188" ctx
  -- _ <- setShadowOffsetX 10.0 ctx
  -- _ <- setShadowOffsetY 10.0 ctx
  -- _ <- setShadowBlur 3.0 ctx
  -- _ <- setShadowColor "#777" ctx

  -- fillPath ctx $ do 
  --   _ <- lsystem initial productions interpret 4 initialState
  --   closePath ctx
  lsystem initial productions interpretTrace 1 initialState
