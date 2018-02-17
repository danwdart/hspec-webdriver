{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances, DeriveDataTypeable, TypeFamilies, CPP, NamedFieldPuns, ScopedTypeVariables, TupleSections #-}
module Test.Hspec.WebDriver.Core where

import Control.Exception (SomeException(..))
import Control.Exception.Lifted (try, onException, throwIO)
import Data.IORef (newIORef, writeIORef, readIORef)
import Data.Typeable (cast)
import Test.Hspec.Core.Spec (Result(..), Example(..))
import Test.Hspec.WebDriver.Types
import Test.Hspec.WebDriver.Util
import qualified Test.WebDriver as W
import Test.WebDriver.Commands
import qualified Test.WebDriver.Config as W
import qualified Test.WebDriver.Session as W


instance Eq multi => Example (WdExample multi) where
    type Arg (WdExample multi) = WdTestSession multi
    evaluateExample (WdPending msg) _ _ _ = return $ Pending msg
    evaluateExample wdExample _ act _ = do
      -- IORefs are gross here but they're necessary to get the results out of 'act'
      prevHadError <- newIORef False
      aborted <- newIORef False

      act $ \testsession -> do
        (tstate', maybeError) <- runAction' wdExample testsession

        -- Running wdTestClose allows the next test to proceed
        wdTestClose testsession tstate'
        whenJust maybeError $ throwIO

        writeIORef aborted (stPrevAborted tstate')
        writeIORef prevHadError (stPrevHadError tstate')

      merr <- readIORef prevHadError
      mabort <- readIORef aborted
      return $ case (merr, mabort) of
          (True, _) -> Pending (Just "Previous example had an error")
          (_, True) -> Pending (Just "Session has been aborted")
          _ -> Success

runAction' :: Eq multi => WdExample multi -> WdTestSession multi -> IO (SessionState multi, Maybe SomeException)
runAction' (WdExample multi (WdOptions {skipRemainingTestsAfterFailure}) wd) testsession = do
  tstate <- wdTestOpen testsession

  let shouldSkip = (stPrevHadError tstate || stPrevAborted tstate) && skipRemainingTestsAfterFailure

  eitherMSess :: Either String W.WDSession <- case lookup multi $ stSessionMap tstate of
    Just s -> return $ Right s
    Nothing -> onException (Right <$> (createWDSession $ stConfig tstate)) (return $ Left "Failed to create session")

  (aborted, (errored, maybeImmediateError), maybeWDSession') <- case eitherMSess of
    Left _ -> return (stPrevAborted tstate, (stPrevHadError tstate, Nothing), Nothing)

    Right wdsession | shouldSkip -> return (stPrevAborted tstate, (stPrevHadError tstate, Nothing), Just wdsession)

    Right wdsession -> W.runWD wdsession $ do
      macterr <- try wd
      case macterr of
        Right () -> W.getSession >>= \session' -> return $ (stPrevAborted tstate, (stPrevHadError tstate, Nothing), Just session')
        Left acterr@(SomeException actex) ->
          case cast actex of
            Just AbortSession -> return (True, (stPrevHadError tstate, Nothing), Nothing)
            Nothing -> return (False, (True, Just acterr), Nothing)

  (tstate', maybeError) <- case (aborted, (errored, maybeImmediateError), maybeWDSession') of
    (True, _, _) -> return (tstate { stSessionMap = [], stPrevAborted = True }, Nothing) -- pass empty list on to the next test so the session is not closed
    (_, (True, Just acterr), _) -> return (tstate { stPrevHadError = True }, Just acterr)
    (_, _, Just wdsession') -> do
      let smap = (multi, wdsession') : filter ((/=multi) . fst) (stSessionMap tstate)
      return (tstate { stSessionMap = smap }, Nothing)

  return (tstate', maybeError)

createWDSession :: W.WDConfig -> IO W.WDSession
createWDSession cfg = do
  s <- W.mkSession cfg
#if MIN_VERSION_webdriver(0,7,0)
  W.runWD s $ createSession $ W.wdCapabilities cfg
#else
  W.runWD s $ createSession [] $ W.wdCapabilities cfg
#endif
