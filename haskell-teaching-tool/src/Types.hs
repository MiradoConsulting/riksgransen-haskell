{-# LANGUAGE DeriveGeneric,
             GeneralizedNewtypeDeriving,
             MultiParamTypeClasses #-}

module Types where

import Data.Char                (toUpper)
import Data.Hashable            (Hashable)
import Data.Text                (Text)
import Data.UUID                (toString)
import Data.UUID.V4             (nextRandom)
import GHC.Generics             (Generic)
import Servant.API.ContentTypes (MimeUnrender, mimeUnrender)
import Servant                  (FromHttpApiData, parseUrlPiece, PlainText)
import Web.FormUrlEncoded       (FromForm)

data Correct = Correct
             | Incorrect
                 deriving Show

newtype ProblemId = ProblemId Text deriving (Eq, Hashable, Show)
instance FromHttpApiData ProblemId where
  parseUrlPiece txt = ProblemId <$> parseUrlPiece txt

newtype ProblemReq = ProblemReq Text deriving Show
instance MimeUnrender PlainText ProblemReq where
  mimeUnrender proxy bs = ProblemReq <$> mimeUnrender proxy bs

newtype UserId = UserId { userId :: Text } deriving (Eq, Generic, Hashable, Show)
instance MimeUnrender PlainText UserId where
  mimeUnrender proxy bs = UserId <$> mimeUnrender proxy bs

instance FromForm UserId
instance FromHttpApiData UserId where
  parseUrlPiece txt = UserId <$> parseUrlPiece txt

newtype ModuleName = ModuleName String

makeModuleName :: IO ModuleName
makeModuleName = ModuleName . ('M':) . map toUpper . filter (/= '-') . toString <$> nextRandom
