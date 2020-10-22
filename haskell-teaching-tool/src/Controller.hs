{-# LANGUAGE DataKinds,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeOperators #-}

module Controller where

import Prelude hiding (div)

import ProblemResp                 (ProblemResp, noIdSubmitted)
import Storage
import Types                       (UserId (..), ProblemId (..), ProblemReq (..))

import           Control.Monad.IO.Class      (liftIO)
import           Data.List                   (sort)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text as T
import           Network.HTTP.Types.Header   (hLocation)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant
import           Servant.HTML.Blaze          (HTML)
import           Text.Blaze.Html5            (Html, html, (!), a, text, div, h4, li, ul)
import           Text.Blaze.Html5.Attributes (href)

type HaskellWorkshopApi = Get '[PlainText] Text

                     :<|> Header "Cookie" UserId :> "problem" :> QueryParam "id" ProblemId
                                                              :> ReqBody '[PlainText] ProblemReq
                                                              :> Post '[JSON] ProblemResp

                     :<|> "itsme" :> ReqBody '[FormUrlEncoded] UserId
                                  :> Post '[HTML] (Headers '[Header "Set-Cookie" Text] Html)

                     :<|> "nav" :> Get '[HTML] Html

                     :<|> "results" :> Get '[HTML] Html

                     :<|> Raw --Only GET or HEAD is supported

redirToBeginner :: Handler a
redirToBeginner = throwError $ err302 { errHeaders = [(hLocation, "/beginner.html")] }

redirToLogin :: Handler a
redirToLogin = throwError $ err302 { errHeaders = [(hLocation, "/login.html")] }

doLogin :: UserId -> Handler (Headers '[Header "Set-Cookie" Text] Html)
doLogin (UserId uid) = pure
                     . addHeader uid
                     . html $ do
                         div "You are now logged in."
                         a "Back" ! href "/"

solve :: (UserId -> ProblemId -> ProblemReq -> IO ProblemResp)
      -> Maybe UserId
      -> Maybe ProblemId
      -> ProblemReq
      -> Handler ProblemResp
solve       _    Nothing                _   _ = redirToLogin
solve       _          _          Nothing   _ = pure noIdSubmitted
solve solvers (Just uid) (Just problemId) req = liftIO $ solvers uid problemId req

navSection :: Html
navSection = html $ do
    h4 "Sections"
    ul $ do
      li $ a ! href "login.html"           $ "log in/out"
      li $ a ! href "beginner.html"        $ "beginner"
      li $ a ! href "functions.html"       $ "functions & type signatures"
      li $ a ! href "datatypes.html"       $ "data types"
      li $ a ! href "patternmatching.html" $ "pattern matching"
      li $ a ! href "listprocessing.html"  $ "list processing"
      li $ a ! href "accumulators.html"    $ "accumulators"
      li $ a ! href "typeclasses.html"     $ "type classes"

runServers :: Storage IO -> (UserId -> ProblemId -> ProblemReq -> IO ProblemResp) -> IO ()
runServers storage solvers = run 8080 haskellWorkshopStatic

    where
    haskellWorkshopStatic :: Application
    haskellWorkshopStatic = simpleCors $ serve (Proxy :: Proxy HaskellWorkshopApi) routes
        where
        routes = redirToBeginner
            :<|> solve solvers
            :<|> doLogin
            :<|> pure navSection
            :<|> results storage
            :<|> serveDirectoryWebApp "frontend"

results :: Storage IO -> Handler Html
results storage = do
    rows <- liftIO $ getSolutions storage
    pure . html $
        mapM_ li (map text . sort . map render $ rows)

    where
    render :: (UserId, ProblemId, ProblemReq, Correct) -> Text
    render (UserId uid, ProblemId pid, ProblemReq _, correct) =
        pid <> " " <> uid <> ": " <> " " <> (T.pack $ show correct)
