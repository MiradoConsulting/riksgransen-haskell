module Main where

import           Controller (runServers)
import qualified Locking    (create)
import qualified Solver     (create)
import qualified Storage    (create)

import Control.Concurrent.Async (wait)
import System.IO                (BufferMode (..), hSetBuffering, stdout)
import Text.Printf              (printf)

port :: Int
port = 8080

main :: IO ()
main = do

    hSetBuffering stdout LineBuffering

    storage <- Storage.create
    lock    <- Locking.create
    solver  <- Solver.create storage lock
    a       <- runServers storage solver port

    printf "Running on port: %d\n" port
    wait a
