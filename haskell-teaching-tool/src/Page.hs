{-# LANGUAGE OverloadedStrings #-}

module Page where

import Types (UserId (..), ProblemId (..), ProblemReq (..), Correct)

import           Data.List                          (sort)
import           Data.Text                          (Text)
import qualified Data.Text as T
import           Prelude                     hiding (div)
import           Text.Blaze.Html5            hiding (map)
import           Text.Blaze.Html5.Attributes hiding (rows)
import           Text.Printf                        (printf)

navSection :: Html
navSection = html $ do
    h4 "Menu"
    ul $
      li $ a ! href "login.html"           $ "log in/out"
    h4 "Sections"
    ul $ do
      li $ a ! href "beginner.html"        $ "beginner"
      li $ a ! href "functions.html"       $ "functions & type signatures"
      li $ a ! href "datatypes.html"       $ "data types"
      li $ a ! href "patternmatching.html" $ "pattern matching"
      li $ a ! href "listprocessing.html"  $ "list processing"
      li $ a ! href "accumulators.html"    $ "accumulators"
      li $ a ! href "typeclasses.html"     $ "type classes"

youAreNowLoggedIn :: Html
youAreNowLoggedIn = html $ do
    div "You are now logged in."
    a "Back" ! href "/"

resultsPage :: [(UserId, ProblemId, ProblemReq, Correct)] -> Html
resultsPage rows =
    html $ mapM_ (li . text) . sort
         $ map render rows

    where
    render :: (UserId, ProblemId, ProblemReq, Correct) -> Text
    render (UserId uid, ProblemId pid, ProblemReq _, correct) =
        T.pack $ printf "%s %s %s" pid uid (show correct)