module Exercises where

import Files
import Prelude

import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Except.Trans (ExceptT(..))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Traversable (traverse, traverseDefault, traverse_)
import Types (Async)

concatFilesCont
  :: forall eff
   . FilePath
  -> FilePath
  -> FilePath
  -> Async (fs :: FS | eff) (Either ErrorCode Unit)
concatFilesCont src1 src2 dest = do
  out <- concatContentsCont
  case out of
    Right concated -> writeFileCont dest concated
    Left err -> pure $ Left err
  where
  concatContentsCont = do
    a <- readFileCont src1
    case a of
      Left aErr -> pure $ Left aErr
      Right contentA -> do
        b <- readFileCont src2
        case b of
          Left bErr -> pure $ Left bErr
          Right contentB -> pure $ Right $ contentA <> contentB

concatFilesContEx :: forall eff. FilePath -> FilePath -> FilePath -> ExceptT ErrorCode (Async (fs :: FS | eff)) Unit
concatFilesContEx src1 src2 dest = do
  a <- readFileContEx src1
  b <- readFileContEx src2
  writeFileContEx dest (a <> b)

-- runContT (runExceptT (concatFilesManyContEx ["a.txt", "b.txt", "c.txt"] "f.txt")) logShow
concatFilesManyContEx :: forall eff. (Array FilePath) -> FilePath -> ExceptT ErrorCode (Async (fs :: FS | eff)) Unit
concatFilesManyContEx xs dest = do
  collect <- traverse readFileContEx xs
  concat <- pure $ foldl (<>) "" collect
  writeFileContEx dest concat

type Milliseconds = Int

foreign import data TIMEOUT :: Effect

foreign import timeoutImpl ::
                forall eff. Fn2 Milliseconds
                  (Unit -> Eff (timeout :: TIMEOUT | eff) Unit)
                  (Eff (timeout :: TIMEOUT | eff) Unit)

setTimeout :: forall eff. Milliseconds -> (Unit -> Eff (timeout :: TIMEOUT | eff) Unit) -> Eff (timeout :: TIMEOUT | eff) Unit
setTimeout time f = runFn2 timeoutImpl time f

setTimeoutCont
  :: forall eff
   . Milliseconds
  -> Async (timeout :: TIMEOUT | eff) Unit
setTimeoutCont time = ContT $ setTimeout time

setTimeoutUsage :: Eff (timeout :: TIMEOUT, console :: CONSOLE) Unit
setTimeoutUsage = do
  _ <- runContT (setTimeoutCont 5000) (\x -> pure $ unit)
  logShow "hi"

