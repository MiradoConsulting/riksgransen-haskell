module Locking where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)

makeLock :: IO (MVar ())
makeLock = newEmptyMVar

takeLock :: MVar () -> IO (MVar ())
takeLock mVar = do
    putMVar mVar ()
    pure mVar

releaseLock :: MVar () -> IO ()
releaseLock mVar = takeMVar mVar
