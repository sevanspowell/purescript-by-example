module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array.Partial (tail)
import Partial.Unsafe (unsafePartial)
import Data.Foldable (sum)
import Math (sqrt)
import Data.Maybe (Maybe(..))
-- import Data.Picture (Point)

gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 m = m
gcd n m = if n > m
            then gcd (n - m) m
            else gcd n (m - n)

gcd' :: Int -> Int -> Int
gcd' n 0 = n
gcd' 0 m = m
gcd' n m | n > m     = gcd' (n - m) m
         | otherwise = gcd' n (m - n)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

binomialCoefficient :: Int -> Int -> Int
binomialCoefficient n 0 = 1
binomialCoefficient 1 1 = 1
binomialCoefficient n k | k > n = 0
                        | otherwise = binomialCoefficient (n - 1) (k - 1) + binomialCoefficient (n - 1) (k)

isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true
isEmpty _ = false

takeFive :: Array Int -> Int
-- Only matches arrays with 5 elements, whose first and second elements are 0
-- and 1 respectively
takeFive [0, 1, a, b, _] = a * b
takeFive _ = 0

-- r character here matches ANY record which has the fields 'first' and 'last',
-- even if it has more fields. It will not match records that have less fields.
-- row polymorphism
showPerson :: forall r. { first :: String, last :: String | r } -> String
showPerson { first: x, last: y } = y <> ", " <> x

type Address = { street :: String, city :: String }

type Person = { name :: String, address :: Address }

-- Can pattern match nested records
livesInLA :: Person -> Boolean
livesInLA { address: { city: "Los Angeles" } } = true
livesInLA _ = false

-- Named patterns
sortPair :: Array Int -> Array Int
sortPair arr@[x, y]
  | x <= y = arr
  | otherwise = [y, x]
sortPair arr = arr -- Ignore arrays larger than two elements

sameCity :: forall a x1 y1 x2 y2. Eq a => { address :: { city :: a | x1 } | y1}
                                       -> { address :: { city :: a | x2 } | y2}
                                       -> Boolean
sameCity p1 p2 = p1.address.city == p2.address.city

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton a _ = a

-- Compute longest suffix which sums to zero
lzs :: Array Int -> Array Int
lzs [] = []
lzs xs = case sum xs of
           0 -> xs
           _ -> lzs (unsafePartial tail xs)

-- Only matches true, if given false, runtime error. This is a partial function,
-- does not necessarily return a value for all inputs. Opposite is a total
-- function.
partialFunction :: Boolean -> Boolean
partialFunction = unsafePartial \true -> true

-- data Point = Point
--   { x :: Number
--   , y :: Number
--   }

-- data Shape
--   = Circle Point Number
--   | Rectangle Point Number Number
--   | Line Point Point
--   | Text Point String

-- exampleLine :: Shape
-- exampleLine = Line p1 p2
--   where
--     p1 :: Point
--     p1 = Point { x: 0.0, y: 0.0 }

--     p2 :: Point
--     p2 = Point { x: 100.0, y: 50.0 }

-- -- showPoint :: Point -> String
-- -- showPoint (Point { x, y }) =
-- --   "(" <> show x <> ", " <> show y <> ")"

-- -- showShape :: Shape -> String
-- -- showShape (Circle c r) =
-- --   "Circle [center: " <> showPoint c <> ", radius:" <> show r <> "]"
-- -- showShape (Rectangle c w h) =
-- --   "Rectange [origin: " <> showPoint c <> ", width: " <> show w <> ", height: " <> show h <> "]"
-- -- showShape (Line start end) =
-- --   "Line [start: " <> showPoint start <> ", end: " <> showPoint end <> "]"
-- -- showShape (Text p text) =
-- --   "Text [origin: " <> showPoint p <> ", text:" <> show text <> "]"

-- circleRadiusTen :: Shape
-- circleRadiusTen = Circle p r
--   where
--     p :: Point
--     p = Point { x: 0.0, y: 0.0 }

--     r :: Number
--     r = 10.0

-- doubleSize :: Shape -> Shape
-- doubleSize (Circle c r) = Circle origin (r*2.0)
-- doubleSize (Rectangle c w h) = Rectangle origin (w*sqrt(2.0)) (h*sqrt(2.0))
-- doubleSize (Line start end) = Line start end
-- doubleSize (Text p text) = Text origin text

-- getText :: Shape -> Maybe String
-- getText (Text _ text) = Just text
-- getText _ = Nothing

-- -- type Picture = Array Shape

-- -- showPicture :: Picture -> Array String
-- -- showPicture = map showShape
