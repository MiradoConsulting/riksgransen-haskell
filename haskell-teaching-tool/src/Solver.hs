{-# LANGUAGE LambdaCase,
             OverloadedStrings,
             ScopedTypeVariables #-}

module Solver (Solver (..), create) where

import           Locking     (Lock (..))
import           ProblemResp
import           Storage     (Storage (..))
import           Types

import           Control.Monad                (mfilter)
import           Data.Functor                 ((<&>))
import           Data.Maybe                   (fromMaybe)
import           Data.Text                    (Text)
import qualified Data.Text as T               (concat, lines, pack, unpack)
import           Data.Typeable                (Typeable)
import           Language.Haskell.Interpreter
import           System.Directory             (createDirectoryIfMissing, removeFile)
import           System.Timeout               (timeout)
import           Test.QuickCheck              (Arbitrary, quickCheckResult, property, output)
import           Text.Printf                  (printf)
import           UnliftIO.Exception           (bracket)

newtype Solver = Solver { runSolver :: UserId
                                    -> ProblemId
                                    -> ProblemReq
                                    -> IO ProblemResp }

create :: Storage IO
       -> Lock
       -> IO Solver
create storage lock = do
    warmup
    pure $ Solver { runSolver = userSolve storage lock }

tempPath :: FilePath
tempPath = "/tmp/programs"

warmup :: IO ()
warmup = do
    putStrLn "Creating tmp directory"
    createDirectoryIfMissing True tempPath
    putStrLn "Warming up!"
    Right _ <- runInterpreter $ do
        setImports ["Prelude"]
        runStmt "putStrLn \"Interpreter ran\""
    return ()

userSolve :: Storage IO
          -> Lock
          -> UserId
          -> ProblemId
          -> ProblemReq
          -> IO ProblemResp
userSolve storage lock u@(UserId uid) pid@(ProblemId problemId) preq@(ProblemReq problemReq) =
    bracket (takeLock lock) (\_ -> releaseLock lock) $ \_ -> do
        printf "User %s attemping problem %s with:\n%s\n" uid  problemId problemReq
        (problemResp, correct) <- solve pid preq
        markSolution storage u pid preq correct
        pure problemResp

solve :: ProblemId -> ProblemReq -> IO (ProblemResp, Correct)

--Please enter an int
solve (ProblemId "enterInteger") pr =
    runTypeCheck pr $ TypeCheck { type_ = undefined :: Integer
                                , hint_ = Nothing
                                }

--Please enter a string
solve (ProblemId "enterString") pr =
    runTypeCheck pr $ TypeCheck { type_ = undefined :: String
                                , hint_ = Just "Hint: Strings are wrapped in double quotes"
                                }

-- Please define a function g which adds 2 to a number
solve (ProblemId "addTwo") pr =
    runProgram pr $ Program { _type          = undefined :: Integer -> Integer
                            , _input         = undefined :: Integer
                            , timeout_micros = 100000
                            , imports        = ["Prelude"]
                            , entry          = "g"
                            , postProgram    = Nothing
                            , mTest          = Just $ \prog i -> prog i == i + 2
                            , _hint          = Just $ T.pack "Hint: Did you define a function called g?"
                            }

-- Please define a function dropSome which drops the first 3 items in a list.
solve (ProblemId "dropSome") pr =
    runProgram pr $ Program { _type          = undefined :: [Integer] -> [Integer]
                            , _input         = undefined :: [Integer]
                            , timeout_micros = 100000
                            , imports        = ["Prelude"]
                            , entry          = "dropSome"
                            , postProgram    = Nothing
                            , mTest          = Just $ \prog i -> prog i == drop 3 i
                            , _hint          = Just $ T.pack "Hint: Try using the function drop"
                            }

solve (ProblemId "basic_5") pr =
    runProgram pr $ Program { _type          = undefined :: Integer -> Integer -> Integer
                            , _input         = undefined :: (Integer, Integer)
                            , timeout_micros = 100000
                            , imports        = ["Prelude"]
                            , entry          = "add"
                            , postProgram    = Nothing
                            , mTest          = Just $ \prog (x, y) -> prog x y == x + y
                            , _hint          = Just $ T.pack "Hint: Function definitions don't need , or ().  Try defining it like 'add x y = ...'"
                            }

--TODO not yet used (add "helper functions" ?)
solve (ProblemId "intermediate_1") pr =
    runProgram pr $ Program { _type          = undefined :: [Integer] -> Maybe Integer
                            , _input         = undefined :: [Integer]
                            , timeout_micros = 100000
                            , imports        = ["Prelude"]
                            , entry          = "smallest"
                            , postProgram    = Nothing
                            , mTest          = Just $ \prog xs -> prog xs == if null xs then Nothing else Just (minimum xs)
                            , _hint          = Just $ T.pack "Hint: Use an accumulator!"
                            }

solve (ProblemId "defineColour") pr =
    runProgram pr $ Program { _type          = undefined :: IO ()
                            , _input         = undefined :: ()
                            , timeout_micros = 100000
                            , imports        = ["Prelude"]
                            , entry          = "main"
                            , postProgram    = Just $ unlines [ "main :: IO ()            "
                                                                , "main = do                "
                                                                , "  let r = Red  :: Colour "
                                                                , "  let b = Blue :: Colour "
                                                                , "  return ()              "
                                                                ]
                            , mTest          = Nothing
                            , _hint          = Nothing
                            }

solve (ProblemId "defineCoord") pr =
    runProgram pr $ Program { _type          = undefined :: IO ()
                            , _input         = undefined :: ()
                            , timeout_micros = 100000
                            , imports        = ["Prelude"]
                            , entry          = "main"
                            , postProgram    = Just $ unlines [ "main :: IO ()                    "
                                                                , "main = do                        "
                                                                , "  let a = Coord2d 1 2   :: Coord "
                                                                , "  let b = Coord3d 1 2 3 :: Coord "
                                                                , "  return ()                      "
                                                                ]
                            , mTest          = Nothing
                            , _hint          = Nothing
                            }

solve (ProblemId "showColour") pr =
    runProgram pr $ Program { _type          = undefined :: IO ()
                            , _input         = undefined :: ()
                            , timeout_micros = 100000
                            , imports        = ["Prelude"]
                            , entry          = "main"
                            , postProgram    = Just $ unlines [ "main :: IO ()                                       "
                                                                    , "main = mapM_ print ([Red, Green, Blue] :: [Colour]) "
                                                                    ]
                            , mTest          = Nothing
                            , _hint          = Nothing
                            }

solve (ProblemId "safeDiv") pr =
    runProgram pr $ Program { _type          = undefined :: Int -> Int -> Maybe Int
                            , _input         = undefined :: (Int, Int)
                            , timeout_micros = 100000
                            , imports        = ["Prelude"]
                            , entry          = "safeDiv"
                            , postProgram    = Nothing
                            , mTest          = Just $ \prog (x, y) ->
                                                            prog x y ==
                                                            ((\d -> x `div` d) <$> mfilter (/=0) (Just y))
                            , _hint          = Just $ T.pack "Try pattern matching against 0 or any other n"
                            }

solve (ProblemId "listLength") pr =
    runProgram pr $ Program { _type          = undefined :: [Integer] -> Integer
                            , _input         = undefined :: [Integer]
                            , timeout_micros = 100000
                            , imports        = ["Prelude"]
                            , entry          = "listLength"
                            , postProgram    = Nothing
                            , mTest          = Just $ \prog xs -> prog xs == fromIntegral (length xs)
                            , _hint          = Just $ T.pack "Try pattern matching against the empty list [] or a list with at least one element (x:xs)"
                            }

solve (ProblemId "abs") pr =
    runProgram pr $ Program { _type          = undefined :: Int -> Int
                            , _input         = undefined :: Int
                            , timeout_micros = 100000
                            , imports        = ["Prelude"]
                            , entry          = "abs"
                            , postProgram    = Nothing
                            , mTest          = Just $ \prog x -> prog x == fromIntegral (abs x)
                            , _hint          = Nothing
                            }

solve (ProblemId "findMax") pr =
    runProgram pr $ Program { _type          = undefined :: [Int] -> Maybe Int
                            , _input         = undefined :: [Int]
                            , timeout_micros = 100000
                            , imports        = ["Prelude"]
                            , entry          = "findMax"
                            , postProgram    = Nothing
                            , mTest          = Just $ \prog xs -> prog xs == if null xs then Nothing else Just $ maximum xs
                            , _hint          = Just $ T.pack "Try pattern matching against the empty list [] or a list with at least one element (x:xs)"
                            }

solve (ProblemId unknown) _ = do
    let eMsg = T.unpack . T.concat $ ["Unknown problem id: ", unknown]
    putStrLn eMsg
    error eMsg

data TypeCheck a = TypeCheck { type_ :: a
                             , hint_ :: !(Maybe Text)
                             }

data Program a i = Program { _type          :: a
                           , _input         :: i
                           , timeout_micros :: !Int
                           , imports        :: ![String]
                           , entry          :: !String
                           , postProgram    :: !(Maybe String)
                           , mTest          :: !(Maybe (a -> (i -> Bool)))
                           , _hint          :: !(Maybe Text)
                           }

runTypeCheck :: forall a. Typeable a => ProblemReq
                                     -> TypeCheck a
                                     -> IO (ProblemResp, Correct)
runTypeCheck (ProblemReq source) (TypeCheck _ mHint) =

    runInterpreter typeCheck <&> \case

        Left (WontCompile es) ->
            let resp = failureHint (map T.pack . concatMap (lines . errMsg) $ es) mHint
            in (resp, Incorrect)

        Right (_r :: a) -> (success, Correct)

    where
    typeCheck = do reset
                   setImports ["Prelude"]
                   interpret (T.unpack source) as

runProgram :: (Arbitrary i, Show i, Typeable a) => ProblemReq
                                                -> Program a i
                                                -> IO (ProblemResp, Correct)
runProgram (ProblemReq source) prog = do

    ModuleName moduleName <- makeModuleName

    let fileName = concat [tempPath, "/", moduleName, ".hs"]

        programMiddle = unlines [ unwords ["module", moduleName, "where"]
                                , ""
                                , T.unpack source
                                , ""
                                , fromMaybe "" (postProgram prog)
                                ]

    writeFile fileName programMiddle

    interpretResult <- runInterpreter $ do
                           reset
                           set [searchPath := [tempPath]]
                           loadModules [fileName]
                           setImports (moduleName:imports prog)
                           interpret (moduleName ++ '.' : entry prog) as

    removeFile fileName

    case interpretResult of
        Left (WontCompile es) -> do
            let resp = failureHint (map T.pack . concatMap (lines . errMsg) $ es) (_hint prog)
            pure (resp, Incorrect)
        Right interpreted ->
          case mTest prog of
              Nothing -> pure (success, Correct)
              Just test -> do
                timeoutResult <- timeout (timeout_micros prog) . quickCheckResult $ property (test interpreted)
                case timeoutResult of
                  Just r -> pure (passedTests . T.lines . T.pack . output $ r, Correct)
                  Nothing -> pure (failure ["Test timed out.  Did you infinite loop?"], Incorrect)
