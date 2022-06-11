module Demo where

import Control.Monad.IOSim
import Control.Monad.Class.MonadSay (MonadSay(..))
import Control.Monad.Class.MonadTimer (MonadDelay(..), DiffTime)
import Control.Tracer
import Control.Monad.Class.MonadSTM (MonadSTM(..))
import Control.Monad.Class.MonadFork (MonadFork(..), labelThisThread)
import Data.Data (Typeable)
import Control.Monad.Class.MonadAsync (MonadAsync(..), AsyncCancelled (AsyncCancelled))
import Control.Monad.Class.MonadThrow (MonadThrow(throwIO), MonadMask)
import Control.Monad (forever)

-- Very simple example
--
example :: MonadSay m => m ()
example = do
  say "Hello World"

-- Very simple example where time passes
--
example2 :: (MonadDelay m, MonadSay m) => m ()
example2 = do
  threadDelay 1000
  example

-- Very simple example where time passes and we have more information about the
-- events
--
data TraceExample = SettingThreadDelay DiffTime
                  | Printing String
                  deriving (Show)

dynamicTracer :: (Show a, Typeable a) => Tracer (IOSim s) a
dynamicTracer = Tracer traceM <> Tracer (say . show)

example3 :: (MonadDelay m, MonadSay m) => Tracer m TraceExample -> m ()
example3 tr = do
  traceWith tr (SettingThreadDelay 1000)
  threadDelay 1000
  traceWith tr (Printing "Hello World")
  say "Hello World"

-- Not so simple example with concurrency
--
example4 :: (MonadFork m, MonadSTM m, MonadDelay m, MonadSay m) => m ()
example4 = do
  result <- newEmptyTMVarIO

  _ <- forkIO $ do
    threadDelay 5
    say "Calculated result!"
    atomically (putTMVar result (42 :: Int))

  say "Waiting..."
  value <- atomically $ takeTMVar result
  say ("The answer is: " ++ show value)

-- Not so simple example with race conditions
--
example5 :: (MonadFork m, MonadSTM m, MonadDelay m, MonadSay m) => m ()
example5 = do
  result <- newEmptyTMVarIO

  _ <- forkIO $ do
    threadDelay 5
    say "Calculated result!"
    atomically (putTMVar result (42 :: Int))

  _ <- forkIO $ do
    threadDelay 5
    say "Calculated result!"
    atomically (putTMVar result 24)

  say "Waiting..."
  value <- atomically $ takeTMVar result
  say ("The answer is: " ++ show value)

-- Not so simple example with race conditions 2
--
example6 :: (MonadFork m, MonadSTM m, MonadSay m) => m ()
example6 = do
  result <- newEmptyTMVarIO

  _ <- forkIO $ do
    labelThisThread "Thread A"
    say "Calculated result!"
    atomically (putTMVar result (42 :: Int))

  _ <- forkIO $ do
    labelThisThread "Thread B"
    say "Calculated result!"
    atomically (putTMVar result 24)

  say "Waiting..."
  value1 <- atomically $ takeTMVar result
  say ("The answer is: " ++ show value1)
  say "Putting 0"
  atomically $ putTMVar result 0
  value2 <- atomically $ takeTMVar result
  say ("The answer is: " ++ show value2)

-- Asynchronous exceptions example
--
example7 :: (MonadAsync m, MonadDelay m, MonadSay m, MonadMask m) => m ()
example7 = do
  withAsync loop $ \_ -> do
    threadDelay 5
    throwIO AsyncCancelled

  where
    importantTask = do
      say "Starting important task"
      threadDelay 2
      say "Finished important task"
    loop = do
      forever $ do
        importantTask
        threadDelay 1
        say "Loop"

