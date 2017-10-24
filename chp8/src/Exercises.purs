module Exercises where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.ST (ST, newSTRef, readSTRef, modifySTRef, runST)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, Error, catchException, error, message)
import Control.Plus (empty)
import Data.Array ((..))
import Data.Int (round, toNumber)
import Data.Maybe
import Data.Tuple
import Math (pow)

countThrows :: Int -> Array (Array Int)
countThrows n = do
  x <- 1 .. 6
  y <- 1 .. 6
  if x + y == n
    then pure [x, y]
    else empty

simulate :: forall eff h. Number -> Number -> Int -> Eff (st :: ST h | eff) Number
simulate x0 v0 time = do
  ref <- newSTRef { x: x0, v: v0 }
  forE 0 (time * 1000) \_ -> do
    _ <- modifySTRef ref \o ->
      { v: o.v - 9.81 * 0.001
      , x: o.x + o.v * 0.001
      }
    pure unit
  final <- readSTRef ref
  pure final.x

simulate' :: Number -> Number -> Number -> Number
simulate' x0 v0 time = runPure (runST (simulate x0 v0 (round time)))

-- inline above
simulate'' :: forall eff h. Number -> Number -> Int -> Number
simulate'' x0 v0 time = runPure $ runST do
  ref <- newSTRef { x: x0, v: v0 }
  forE 0 (time * 1000) \_ -> do
    _ <- modifySTRef ref \o ->
      { v: o.v - 9.81 * 0.001
      , x: o.x + o.v * 0.001
      }
    pure unit
  final <- readSTRef ref
  pure final.x

safeDivide :: forall e. Int -> Int -> Eff (exception :: EXCEPTION | e) (Maybe Int)
safeDivide _ 0 = throwException $ error "Divide by zero"
safeDivide a b = pure (Just (a / b))

safeDivideTest :: Int -> Int -> Eff (console :: CONSOLE) Unit
safeDivideTest a b = catchException (logShow) $ safeDivide a b >>= (logShow)

generatePoint :: forall eff. Eff (random :: RANDOM | eff) (Tuple Number Number)
generatePoint = do
  x <- random
  y <- random
  pure (Tuple x y)

-- Based off Becky Conning's answer: https://github.com/beckyconning/purescript-by-example/blob/master/chapter8/src/Exercises.purs
estimatePi :: Int -> Eff (random :: RANDOM) Number
estimatePi n = runST (do
  pointsInCircleRef <- newSTRef 0
  forE 0 n \_ -> do
    point <- generatePoint
    _ <- modifySTRef pointsInCircleRef ((+) $ if pointIsInUnitCircle point then 1 else 0)
    pure (unit)
  pointsInCircleCount <- readSTRef pointsInCircleRef
  pure ((4.0 * toNumber(pointsInCircleCount)) / toNumber(n)))
  where
    pointIsInUnitCircle (Tuple x y) = (((x - 0.5) `pow` 2.0) + ((y - 0.5) `pow` 2.0)) < (0.5 `pow` 2.0)
