module ProjectSol where

import Control.Monad.Class.MonadSay (MonadSay(..))
import Control.Monad.Class.MonadTimer (MonadTimer (timeout))
import Control.Tracer
import Control.Monad.Class.MonadSTM (MonadSTM(..))
import Control.Monad.Class.MonadFork (labelThisThread, MonadThread)
import Control.Monad.Class.MonadAsync (MonadAsync(..))

-- Small project here:
--

data LockState = Free | Reading Int | Writing
  deriving (Eq, Show)

type Lock m a = TVar m (a, LockState)

data RWEvents a = StartReading Int
                | Read a
                | StopReading
                | StartWriting
                | Wrote a
                | StopWriting
                deriving (Show)

startReading :: MonadSTM m => Lock m a -> Tracer m (RWEvents a) -> m a
startReading lock tracer = do
  (a, n) <- atomically $ do
    (a, st) <- readTVar lock
    n <- case st of
      Free      -> writeTVar lock (a, Reading 1) >> return 1
      Reading n -> writeTVar lock (a, Reading (succ n)) >> return (succ n)
      Writing   -> retry
    return (a, n)
  traceWith tracer (StartReading n)
  traceWith tracer (Read a)
  return a

stopReading :: MonadSTM m => Lock m a -> Tracer m (RWEvents a) -> m ()
stopReading lock tracer = do
  atomically $ do
    (a, st) <- readTVar lock
    case st of
      Reading n | n > 0 -> writeTVar lock (a, Reading (pred n))
                | otherwise -> error "stopReading: no readers"
      Free      -> error "stopReading: lock in free state"
      Writing   -> error "stopReading: lock in writing state"
  traceWith tracer StopReading

startWriting :: MonadSTM m => Lock m a -> Tracer m (RWEvents a) -> m a
startWriting lock tracer = do
  a <- atomically $ do
    (a, st) <- readTVar lock
    case st of
      Free      -> writeTVar lock (a, Writing)
      Reading 0 -> retry
      Reading _ -> retry
      Writing   -> retry
    return a
  traceWith tracer StartWriting
  return a

stopWriting :: MonadSTM m => Lock m a -> Tracer m (RWEvents a) -> a -> m ()
stopWriting lock tracer newA = do
  atomically $ do
    (_, st) <- readTVar lock
    case st of
      Writing   -> writeTVar lock (newA, Free)
      Free      -> error "stopWriting: lock in free state"
      Reading _ -> error "stopWriting: lock in reading state"
  traceWith tracer StopWriting
  traceWith tracer (Wrote newA)

writerThread :: (MonadSTM m, MonadThread m)
             => Lock m a -> Tracer m (RWEvents a) -> (a -> a) -> m ()
writerThread lock tracer doStuff = do
  labelThisThread "WriterThread"
  a <- startWriting lock tracer
  stopWriting lock tracer (doStuff a)

readerThread :: (MonadSTM m, MonadThread m)
             => Lock m a -> Tracer m (RWEvents a) -> (a -> m b) -> m ()
readerThread lock tracer doStuff = do
  labelThisThread "ReaderThread"
  a <- startReading lock tracer
  _ <- doStuff a
  stopReading lock tracer

newLock :: MonadSTM m => a -> m (Lock m a)
newLock a = newTVarIO (a, Free)

main :: (MonadAsync m, MonadTimer m, MonadSTM m, MonadSay m)
     => Tracer m (RWEvents String) -> m (Maybe ())
main tracer = do
  lock <- newLock ""
  timeout 20 $
    withAsync (writerThread lock tracer (++ "test")) $ \_ ->
      readerThread lock tracer say
