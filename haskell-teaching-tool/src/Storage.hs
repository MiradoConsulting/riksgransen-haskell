module Storage where

import Types

import           Control.Concurrent.STM      (atomically)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           ListT                       (toList)
import qualified StmContainers.Map      as M

data Storage m =
    Storage { markSolution :: UserId -> ProblemId -> ProblemReq -> Correct -> m ()
            , getSolutions :: m [(UserId, ProblemId, ProblemReq, Correct)]
            }

newtype StorageImpl =
    StorageImpl (M.Map (UserId, ProblemId) (ProblemReq, Correct))

create :: MonadIO m => IO (Storage m)
create = do
    storageImpl <- StorageImpl <$> M.newIO
    pure Storage { markSolution = markSolutionImpl storageImpl
                 , getSolutions = getSolutionsImpl storageImpl
                 }

    where
    markSolutionImpl :: MonadIO m => StorageImpl
                                  -> UserId
                                  -> ProblemId
                                  -> ProblemReq
                                  -> Correct
                                  -> m ()
    markSolutionImpl (StorageImpl store) uid pid problemReq correct =
        liftIO . atomically $ M.insert (problemReq, correct) (uid, pid) store

    getSolutionsImpl :: MonadIO m => StorageImpl
                                  -> m [(UserId, ProblemId, ProblemReq, Correct)]
    getSolutionsImpl (StorageImpl store) = liftIO $ do
        xs <- atomically . toList . M.listT $ store
        pure . map (\((a,b), (c,d)) -> (a,b,c,d)) $ xs
