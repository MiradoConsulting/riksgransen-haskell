{-# LANGUAGE OverloadedStrings #-}

module Cookies ( setCookie
               , deleteCookie
               ) where

import Data.Binary.Builder              (toLazyByteString)
import Data.ByteString                  (ByteString)
import Data.ByteString.Lazy             (toStrict)
import Data.Text                        (Text)
import Data.Text.Encoding               (encodeUtf8)
import Data.Time.Calendar               (Day (..))
import Data.Time.Clock                  (DiffTime, UTCTime (..), secondsToDiffTime)
import Network.HTTP.Types.Header        (HeaderName, hSetCookie)
import Web.Cookie                hiding (Cookies)

setCookie :: Text
          -> Text
          -> (HeaderName, ByteString)
setCookie k v =

    let cookie = toStrict
               . toLazyByteString
               . renderSetCookie
               $ defaultSetCookie { setCookieName     = encodeUtf8 k
                                  , setCookieHttpOnly = True
                                  , setCookieSecure   = False
                                  , setCookieMaxAge   = Just twoDays
                                  , setCookiePath     = Just "/"
                                  , setCookieSameSite = Just sameSiteStrict
                                  , setCookieValue    = encodeUtf8 v
                                  }

    in (hSetCookie, cookie)
    where
    twoDays :: DiffTime
    twoDays = secondsToDiffTime 172800

deleteCookie :: Text
             -> (HeaderName, ByteString)
deleteCookie k =

    let cookie = toStrict
               . toLazyByteString
               . renderSetCookie
               $ defaultSetCookie { setCookieName     = encodeUtf8 k
                                  , setCookieHttpOnly = True
                                  , setCookieSecure   = False
                                  , setCookieExpires  = Just earliestTime
                                  , setCookiePath     = Just "/"
                                  , setCookieSameSite = Just sameSiteStrict
                                  , setCookieValue    = "deleted"
                                  }

    in (hSetCookie, cookie)

    where
    earliestTime :: UTCTime
    earliestTime = UTCTime (ModifiedJulianDay 0) 0
