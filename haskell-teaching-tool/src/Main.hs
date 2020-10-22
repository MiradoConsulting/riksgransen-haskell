module Main where

import Controller (runServers)
import Locking    (makeLock, takeLock, releaseLock)
import Solvers    (userSolve, warmup)
import Storage

main :: IO ()
main = do
    warmup
    lock <- makeLock
    storage <- makeStorage
    runServers storage (userSolve storage (takeLock lock) releaseLock)
