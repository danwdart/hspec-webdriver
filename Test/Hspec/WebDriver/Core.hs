{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances, DeriveDataTypeable, TypeFamilies, CPP, NamedFieldPuns, ScopedTypeVariables, TupleSections, InstanceSigs #-}
module Test.Hspec.WebDriver.Core where

import Control.Concurrent
import Control.Exception (SomeException(..))
import Control.Exception.Lifted (try, onException, throwIO)
import Control.Monad
import Data.IORef (newIORef, writeIORef, readIORef)
import Data.Typeable (cast)
import GHC.Stack
import Test.Hspec
import qualified Test.Hspec as H
import Test.Hspec.Core.Spec (Result(..), Example(..), Item(..), fromSpecList, runSpecM, Params, ProgressCallback)
import Test.Hspec.WebDriver.Types
import Test.Hspec.WebDriver.Util
import qualified Test.WebDriver as W
import Test.WebDriver.Commands
import qualified Test.WebDriver.Config as W
import qualified Test.WebDriver.Session as W


instance Eq multi => Example (WdExample multi) where
  type Arg (WdExample multi) = WdTestSession multi

  evaluateExample :: (HasCallStack) => WdExample multi -> Params -> (ActionWith (Arg (WdExample multi)) -> IO ()) -> ProgressCallback -> IO Result
  evaluateExample (WdPending msg) _ _ _ = return $ Pending msg
  evaluateExample wdExample _ act _ = do
    -- IORefs are gross here but they're necessary to get the results out of 'act'
    prevHadError <- newIORef False
    aborted <- newIORef False

    act $ \sessionState -> do
      (tstate', maybeError, skipped) <- runAction' wdExample sessionState
      whenJust maybeError throwIO

      writeIORef aborted (stPrevAborted tstate')
      writeIORef prevHadError (skipped && stPrevHadError tstate')

    merr <- readIORef prevHadError
    mabort <- readIORef aborted
    return $ case (merr, mabort) of
        (True, _) -> Pending (Just "Previous example had an error")
        (_, True) -> Pending (Just "Session has been aborted")
        _ -> Success

runAction' :: (HasCallStack, Eq multi) => WdExample multi -> WdTestSession multi -> IO (WdTestSession multi, Maybe SomeException, Bool)
runAction' (WdPending _) _ = error "runAction called on a WdPending"
runAction' (WdExample multi (WdOptions {skipRemainingTestsAfterFailure}) wdAction) tstate = do
  let skip = (stPrevHadError tstate || stPrevAborted tstate) && skipRemainingTestsAfterFailure

  eitherMSess :: Either String W.WDSession <- modifyMVar (stSessionMap tstate) $ \items ->
    case lookup multi items of
      Just s -> return (items, Right s)
      Nothing -> flip onException (return (items, Left "Failed to create session")) $ do
        newSession <- createWDSession $ stConfig tstate
        return ((multi, newSession) : items, Right newSession)

  (aborted, (errored, maybeImmediateError), maybeWDSession') <- case eitherMSess of
    Left _ -> return (stPrevAborted tstate, (stPrevHadError tstate, Nothing), Nothing)

    Right wdsession | skip -> return (stPrevAborted tstate, (stPrevHadError tstate, Nothing), Just wdsession)

    Right wdsession -> W.runWD wdsession $ do
      macterr <- try (wdAction tstate)
      case macterr of
        Right () -> W.getSession >>= \session' -> return (stPrevAborted tstate, (stPrevHadError tstate, Nothing), Just session')
        Left acterr@(SomeException actex) ->
          case cast actex of
            Just AbortSession -> return (True, (stPrevHadError tstate, Nothing), Nothing)
            Nothing -> return (False, (True, Just acterr), Nothing)

  (tstate', maybeError) <- case (aborted, (errored, maybeImmediateError), maybeWDSession') of
    (True, _, _) -> return (tstate { stPrevAborted = True }, Nothing)
    (_, (True, Just acterr), _) -> return (tstate { stPrevHadError = True }, Just acterr)
    (_, _, Just wdsession') -> return (tstate, Nothing)

  return (tstate', maybeError, skip)

createWDSession :: (HasCallStack) => W.WDConfig -> IO W.WDSession
createWDSession cfg = do
  s <- W.mkSession cfg
#if MIN_VERSION_webdriver(0,7,0)
  W.runWD s $ createSession $ W.wdCapabilities cfg
#else
  W.runWD s $ createSession [] $ W.wdCapabilities cfg
#endif



--------------------------------------------------------------------------------
-- Internal Test Runner
--------------------------------------------------------------------------------

-- | Convert a single test item to a generic item by providing it with the WdTestSession.
procSpecItem :: (HasCallStack) => t -> Item t -> Item ()
procSpecItem sessionState item = item { itemExample = \p act progress -> itemExample item p (act . act') progress }
  where act' f () = f sessionState

-- | Convert a spec tree of test items to a spec tree of generic items by creating a single session for
-- the entire tree.
-- Close all sessions at the end of the tests.
procTestSession :: (HasCallStack) => W.WDConfig -> W.Capabilities -> SpecWith (WdTestSession multi) -> Spec
procTestSession cfg cap spec = do
  sessionsVar <- runIO $ newMVar []

  let closeSessions = void $ withMVar sessionsVar $ \pairs -> sequence [W.runWD y W.closeSession | (_, y) <- pairs]

  trees <- runIO $ runSpecM $ H.afterAll_ closeSessions spec

  let initialWdTestSession = WdTestSession sessionsVar False False (cfg {W.wdCapabilities = cap})
  fromSpecList $ mapNormal initialWdTestSession (procSpecItem initialWdTestSession) trees
