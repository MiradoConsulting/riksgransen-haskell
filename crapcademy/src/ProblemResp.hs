{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             OverloadedStrings #-}

module ProblemResp where

import Data.Aeson   (ToJSON)
import Data.Text    (Text)
import GHC.Generics (Generic)

data ProblemResp = ProblemResp { successful :: !Bool
                               , msgs       :: ![Text]
                               , hint       :: !(Maybe Text)
                               } deriving (Generic, ToJSON)

success :: ProblemResp
success = ProblemResp { successful = True
                      , msgs       = ["success"]
                      , hint       = Nothing
                      }

passedTests :: [Text] -> ProblemResp
passedTests ms = ProblemResp { successful = True
                             , msgs       = ms
                             , hint       = Nothing
                             }

failure :: [Text] -> ProblemResp
failure ms = ProblemResp { successful = False
                         , msgs       = ms
                         , hint       = Nothing
                         }

failureHint :: [Text] -> Maybe Text -> ProblemResp
failureHint ms mHint = ProblemResp { successful = False
                                   , msgs       = ms
                                   , hint       = mHint
                                   }

noIdSubmitted :: ProblemResp
noIdSubmitted = undefined
