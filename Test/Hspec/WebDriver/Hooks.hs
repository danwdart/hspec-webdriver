{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances, DeriveDataTypeable, TypeFamilies, CPP, NamedFieldPuns, ScopedTypeVariables, TupleSections #-}
module Test.Hspec.WebDriver.Hooks where

import Control.Concurrent.MVar
import Control.Exception (SomeException(..))
import Control.Exception.Lifted (try, throwIO, finally)
import Test.Hspec
import Test.Hspec.Core.Spec (Result(..))
import Test.Hspec.WebDriver.Types

import Test.Hspec.WebDriver.Core
import Test.Hspec.WebDriver.Util

-- | Run a custom action before every spec item.
before :: (Eq multi) => WdExample multi -> SpecWith (WdTestSession multi) -> SpecWith (WdTestSession multi)
before ex = beforeWith $ combineFn ex

-- | Run a custom action before the first spec item.
beforeAll :: (Eq multi) => WdExample multi -> SpecWith (WdTestSession multi) -> SpecWith (WdTestSession multi)
beforeAll ex spec = do
  mvar <- runIO (newMVar Empty)
  beforeWith (\testsession -> (memoize mvar (combineFn ex testsession))) spec

-- | Run a custom action after every spec item.
-- Currently swallows errors
after :: (Eq multi) => WdExample multi -> SpecWith (WdTestSession multi) -> SpecWith (WdTestSession multi)
after ex = aroundWith $ \initialAction testsession -> finally (initialAction testsession) (runAction' ex testsession) >> return ()

combineFn :: (Eq multi) => WdExample multi -> WdTestSession multi -> IO (WdTestSession multi)
combineFn ex = (\testsession -> do
                   (tstate', maybeError, skipped) <- runAction' ex testsession
                   whenJust maybeError throwIO
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
    Failed _ -> throwIO (Pending (Just "exception in beforeAll-hook (see previous failure)"))
  either throwIO return result
