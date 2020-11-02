module Main where

import           Controller (runServers)
import qualified Locking    (create)
import qualified Solver     (create)
import qualified Storage    (create)

main :: IO ()
main = do
    storage <- Storage.create
    lock    <- Locking.create
    solver  <- Solver.create storage lock
    runServers storage solver
