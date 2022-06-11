{-# LANGUAGE TypeApplications #-}

module ProjectSolTest where

import Test.QuickCheck (Arbitrary(..), Property, counterexample)

import ProjectSol
import Control.Tracer (Tracer)
import Control.Monad.Class.MonadSay (MonadSay(say))
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadAsync (replicateConcurrently_, MonadAsync, Concurrently (Concurrently, runConcurrently))
import Control.Monad.IOSim (runSimTrace, selectTraceEventsDynamic, ppTrace_, traceSelectTraceEventsDynamic)
import qualified Data.List.Trace as Trace
import Demo (dynamicTracer)

data RWEnv = RWEnv { nReaders :: Int
                   , nWriters :: Int
                   , toWrite :: [String]
                   }
                   deriving (Eq, Show)

instance Arbitrary RWEnv where
  arbitrary = RWEnv <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (RWEnv r w tw) = RWEnv <$> shrink r <*> shrink w <*> shrink tw

runRW :: (MonadSay m, MonadSTM m, MonadThread m, MonadAsync m)
      => RWEnv -> Tracer m (RWEvents String) -> m ()
runRW (RWEnv r w tw) trace = do
  lock <- newLock ""
  let readers = replicateConcurrently_ r (readerThread lock trace say)
      writers = runConcurrently
              $ foldMap Concurrently
                       (writerThread lock trace . const <$> take w tw)
  writers
  readers

-- | No writer should be able to write while someone is reading
--
noWriteWhileRead :: RWEnv -> Property
noWriteWhileRead rwenv =
  let trace = runSimTrace (runRW rwenv dynamicTracer)
      rwTrace = selectTraceEventsDynamic @() @(RWEvents String) trace
   in counterexample (ppTrace_ trace)
    $ prop rwTrace Free
  where
    prop :: [RWEvents a] -> LockState -> Bool
    prop [] _                           = True
    prop ((StartReading n) : t) Free    = prop t (Reading n)
    prop (StopReading : t) (Reading _)  = prop t Free
    prop (StartWriting : _) (Reading _) = False
    prop (_ : t) st                     = prop t st

-- | No 2 writers should be able to write at the same time
--
noWriteWhileWrite :: RWEnv -> Property
noWriteWhileWrite rwenv =
  let trace = runSimTrace (runRW rwenv dynamicTracer)
      rwTrace = selectTraceEventsDynamic @() @(RWEvents String) trace
   in counterexample (Trace.ppTrace show show (traceSelectTraceEventsDynamic @_ @(RWEvents String) trace))
    $ prop rwTrace Free
  where
    prop :: [RWEvents a] -> LockState -> Bool
    prop [] _                       = True
    prop (StartWriting : t) Free    = prop t Writing
    prop (StartWriting : _) Writing = False
    prop (StopWriting : t) Writing  = prop t Free
    prop (_ : t) st                 = prop t st

-- | If the last writer writes X then the first reader must read X
--
readConsistent :: RWEnv -> Property
readConsistent rwenv =
  let trace = runSimTrace (runRW rwenv dynamicTracer)
      rwTrace = selectTraceEventsDynamic @() @(RWEvents String) trace
   in counterexample (Trace.ppTrace show show (traceSelectTraceEventsDynamic @_ @(RWEvents String) trace))
    $ prop rwTrace ""
  where
    prop :: [RWEvents String] -> String -> Bool
    prop [] _              = True
    prop (Read s : t) ps   = s == ps && prop t ps
    prop ((Wrote s) : t) _ = prop t s
    prop (_ : t) ps        = prop t ps
