module Locking ( Lock (..)
               , create
               ) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)

data Lock =
    Lock { takeLock    :: IO ()
         , releaseLock :: IO ()
         }

create :: IO Lock
create = do
    mvar <- newEmptyMVar
    pure $ Lock { takeLock    = takeLockImpl mvar
                , releaseLock = releaseLockImpl mvar
                }

takeLockImpl :: MVar () -> IO ()
takeLockImpl mVar = putMVar mVar ()

releaseLockImpl :: MVar () -> IO ()
releaseLockImpl = takeMVar
