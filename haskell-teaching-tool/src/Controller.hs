{-# LANGUAGE DataKinds,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeOperators #-}

module Controller (runServers) where

import Cookies     (deleteCookie, setCookie)
import Page        (navSection, resultsPage)
import ProblemResp (ProblemResp)
import Solver      (Solver (..))
import Storage     (Storage (..))
import Types       (ProblemId, ProblemReq, UserId (..))

import           Control.Monad.IO.Class      (liftIO)
import           Data.Text                   (Text)
import           Network.HTTP.Types.Header   (hLocation)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant
import           Servant.HTML.Blaze          (HTML)
import           Text.Blaze.Html5            (Html)
import           Text.Printf                 (printf)

type HaskellWorkshopApi = AppApi
                     :<|> StaticApi

type AppApi =

            -- Login
            "login" :> ReqBody '[FormUrlEncoded] UserId
                    :> Post '[HTML] (Headers '[Header "Set-Cookie" Text] Html)

            -- Logout
            :<|> "logout" :> Post '[HTML] (Headers '[Header "Set-Cookie" Text] Html)

            -- Submit problem
            :<|> Header "Cookie" UserId :> "problem"
                                        :> QueryParam "id" ProblemId
                                        :> ReqBody '[PlainText] ProblemReq
                                        :> Post '[JSON] ProblemResp

type StaticApi =

            -- Redirect to beginner
            Get '[HTML] Html

            -- Nav section
            :<|> "nav" :> Get '[HTML] Html

            -- Results
            :<|> "results" :> Get '[HTML] Html

            -- Raw files (Only GET or HEAD is supported)
            :<|> Raw

haskellWorkshopApp :: Storage IO
                   -> Solver
                   -> Application
haskellWorkshopApp storage solver =
    simpleCors $ serve (Proxy :: Proxy HaskellWorkshopApi)
               $ appRoutes :<|> staticRoutes

    where
    appRoutes = login
           :<|> logout
           :<|> submitProblem solver

    staticRoutes = redirToBeginner
              :<|> pure navSection
              :<|> results storage
              :<|> serveDirectoryWebApp "frontend"

login :: UserId
      -> Handler (Headers '[Header "Set-Cookie" Text] Html)
login (UserId uid) = do
    liftIO $ printf "User logged in: %s\n" uid
    throwError err302 { errHeaders = [ setCookie "userId" uid
                                     , (hLocation,  "/beginner.html")
                                     ] }

logout :: Handler (Headers '[Header "Set-Cookie" Text] Html)
logout = throwError err302 { errHeaders = [ deleteCookie "userId"
                                          , (hLocation, "/login.html")
                                          ]
                           , errBody = "Not logged in"
                           }

submitProblem :: Solver
              -> Maybe UserId
              -> Maybe ProblemId
              -> ProblemReq
              -> Handler ProblemResp

submitProblem _ Nothing _ _ =
    throwError err302 { errHeaders = [(hLocation, "/login.html")]
                      , errBody = "Not logged in"
                      }

submitProblem _ _ Nothing _ =
    throwError $ err400 { errBody = "No problem \"id\" supplied!" }

submitProblem solver (Just uid) (Just pid) preq =
    liftIO $ runSolver solver uid pid preq

redirToBeginner :: Handler a
redirToBeginner = throwError $ err302 { errHeaders = [(hLocation, "/beginner.html")] }

results :: Storage IO -> Handler Html
results storage = liftIO $ resultsPage <$> getSolutions storage

runServers :: Storage IO
           -> Solver
           -> IO ()
runServers storage solver =
    run 8080 (haskellWorkshopApp storage solver)
