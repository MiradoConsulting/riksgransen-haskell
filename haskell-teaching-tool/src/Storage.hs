module Storage where

import Types

import           Control.Concurrent.STM      (atomically)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           ListT                       (toList)
import qualified StmContainers.Map      as M

data Correct = Correct | Incorrect deriving Show

data Storage m = Storage { markSolution :: UserId -> ProblemId -> ProblemReq -> Correct -> m ()
                         , getSolutions :: m [(UserId, ProblemId, ProblemReq, Correct)]
                         }

data StorageImpl = StorageImpl (M.Map (UserId, ProblemId) (ProblemReq, Correct))

makeStorage :: MonadIO m => IO (Storage m)
makeStorage = do
    storageImpl <- StorageImpl <$> M.newIO
    pure Storage { markSolution = markSolutionImpl storageImpl
                 , getSolutions = getSolutionsImpl storageImpl
                 }

    where
    markSolutionImpl :: MonadIO m => StorageImpl -> UserId -> ProblemId -> ProblemReq -> Correct -> m ()
    markSolutionImpl (StorageImpl store) userId problemId problemReq correct =
        liftIO . atomically $ M.insert (problemReq, correct) (userId, problemId) store

    getSolutionsImpl :: MonadIO m => StorageImpl -> m [(UserId, ProblemId, ProblemReq, Correct)]
    getSolutionsImpl (StorageImpl store) = liftIO $ do
        xs <- atomically . toList . M.listT $ store
        pure . map (\((a,b), (c,d)) -> (a,b,c,d)) $ xs
