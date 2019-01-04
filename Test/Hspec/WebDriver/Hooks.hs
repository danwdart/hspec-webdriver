{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances, DeriveDataTypeable, TypeFamilies, CPP, NamedFieldPuns, ScopedTypeVariables, TupleSections #-}
module Test.Hspec.WebDriver.Hooks (
  before
  , beforeAll
  , after
  , afterAll
  , aroundWith
  ) where

import Control.Concurrent.MVar
import Control.Exception (SomeException(..))
import Control.Exception.Lifted (try, throwIO)
import Control.Monad
import Data.String.Interpolate.IsString
import Test.Hspec (SpecWith, runIO)
import qualified Test.Hspec as H
import Test.Hspec.Core.Spec
import Test.Hspec.WebDriver.Types

import Test.Hspec.WebDriver.Core
import Test.Hspec.WebDriver.Util

-- | Run a custom action before every spec item.
before :: (Eq multi) => WdExample multi -> SpecWith (WdTestSession multi) -> SpecWith (WdTestSession multi)
before ex = H.beforeWith $ combineFn ex

-- | Run a custom action before the first spec item.
beforeAll :: (Eq multi) => WdExample multi -> SpecWith (WdTestSession multi) -> SpecWith (WdTestSession multi)
beforeAll ex spec = do
  mvar <- runIO (newMVar Empty)
  H.beforeWith (\testsession -> (memoize mvar (combineFn ex testsession))) spec

-- | Run a custom action after every spec item.
after :: (Eq multi) => WdExample multi -> SpecWith (WdTestSession multi) -> SpecWith (WdTestSession multi)
after ex = H.after $ \testsession -> combineFn ex testsession >> return ()

-- | Run a custom action after the last spec item.
afterAll :: (Eq multi) => WdExample multi -> SpecWith (WdTestSession multi) -> SpecWith (WdTestSession multi)
afterAll ex = H.afterAll (\testsession -> void $ combineFn ex testsession)

aroundWith :: (Eq multi) => (IO () -> (WdExample multi)) -> SpecWith (WdTestSession multi) -> SpecWith (WdTestSession multi)
aroundWith wrapper = H.aroundWith $ \runTest -> \testsession -> do
  void $ combineFn (wrapper (runTest testsession)) testsession

combineFn :: (Eq multi) => WdExample multi -> WdTestSession multi -> IO (WdTestSession multi)
combineFn ex = (\testsession -> do
                   (_session', maybeError, _skip) <- runAction' ex testsession
                   whenJust maybeError $ \e -> do
                     putStrLn [i|Exception in combineFn: #{e}|]
                     throwIO e
                   return testsession)

data Memoized a = Empty
                | Memoized a
                | Failed SomeException

memoize :: MVar (Memoized a) -> IO a -> IO a
memoize mvar action = do
  result <- modifyMVar mvar $ \ma -> case ma of
    Empty -> do
      a <- try action
      return (either Failed Memoized a, a)
    Memoized a -> return (ma, Right a)
    Failed _ -> throwIO (Pending Nothing (Just "exception in beforeAll-hook (see previous failure)"))
  either throwIO return result
