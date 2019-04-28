{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             OverloadedStrings #-}

module Everything where

import           Control.Concurrent               (threadDelay)
import           Control.Concurrent.Async
import           Data.Aeson                       (FromJSON, ToJSON, decode, encode)
import           Data.ByteString.Lazy             (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import           GHC.Generics                     (Generic)

-- aeson --

data MyThing = MyThing { someString :: String
                       , someInt    :: Int
                       } deriving (Generic, ToJSON, FromJSON)

encodeMyThing :: MyThing -> ByteString
encodeMyThing myThing = encode myThing

decodeMyThing :: ByteString -> Maybe MyThing
decodeMyThing bs = decode bs

-- async --

runTwoJobs :: IO (String, Int)
runTwoJobs = do
    a1 <- async jobOne
    a2 <- async jobTwo
    result1 <- wait a1
    result2 <- wait a2
    return (result1, result2)

    where
    jobOne :: IO String
    jobOne = do
        threadDelay 1000000
        return ("Hello" ++ ", World")

    jobTwo :: IO Int
    jobTwo = do
        threadDelay 1000000
        return (2 + 2)

-- bytestring --
someByteString :: ByteString
someByteString = L8.unwords
               . map L8.reverse
               . L8.words
               $ "one two three"
