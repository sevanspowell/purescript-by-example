module Parallel where

import Prelude
import Control.Apply (lift2)
import Control.Monad.Cont.Trans (runContT)
import Control.Monad.Eff.Console (logShow)
import Control.Parallel (parallel, sequential)
import Files (readFileCont)

main = flip runContT logShow do
  sequential $
    lift2 append
      <$> parallel (readFileCont "/tmp/1.txt")
      <*> parallel (readFileCont "/tmp/2.txt")
