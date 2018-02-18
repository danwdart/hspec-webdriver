{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances, DeriveDataTypeable, TypeFamilies, CPP, NamedFieldPuns, ScopedTypeVariables, TupleSections #-}
-- | Write hspec tests that are webdriver tests, automatically managing the webdriver sessions.
--
-- This module re-exports functions from "Test.Hspec" and "Test.WebDriver.Commands" and it is
-- intended that you just import @Test.Hspec.WebDriver@.  If you need to import @Test.Hspec@ or
-- @Test.WebDriver@, you should do so using a qualified import.
--
-- >{-# LANGUAGE OverloadedStrings #-}
-- >module XKCD where
-- >
-- >import Test.Hspec.WebDriver
-- >
-- >allBrowsers :: [Capabilities]
-- >allBrowsers = [firefoxCaps, chromeCaps, ieCaps]
-- >
-- >browsersExceptIE :: [Capabilities]
-- >browsersExceptIE = [firefoxCaps, chromeCaps]
-- >
-- >main :: IO ()
-- >main = hspec $
-- >    describe "XKCD Tests" $ do
-- >
-- >        session "for 327" $ using allBrowsers $ do
-- >            it "opens the page" $ runWD $
-- >                openPage "http://www.xkcd.com/327/"
-- >            it "checks hover text" $ runWD $ do
-- >                e <- findElem $ ByCSS "div#comic > img"
-- >                e `shouldBeTag` "img"
-- >                e `shouldHaveAttr` ("title", "Her daughter is named Help I'm trapped in a driver's license factory.")
-- >
-- >        parallel $ session "for 303" $ using browsersExceptIE $ do
-- >            it "opens the page" $ runWD $
-- >                openPage "http://www.xkcd.com/303/"
-- >            it "checks the title" $ runWD $ do
-- >                e <- findElem $ ById "ctitle"
-- >                e `shouldBeTag` "div"
-- >                e `shouldHaveText` "Compiling"
--
-- The above code assumes selenium-server-standalone is running on @127.0.0.1:4444@ at path
-- @\/wd\/hub@ (this is the default).
module Test.Hspec.WebDriver (
  -- * Webdriver Example
    WdExample(..)
  , WdOptions (..)
  , runWD
  , runWDOptions
  , runWDWith
  , runWDWithOptions
  , pending
  , pendingWith
  , example

  -- * Webdriver Sessions
  , session
  , sessionWith
  , inspectSession
  , using
  , WdTestSession

  -- * Default Capabilities
  , module Test.Hspec.WebDriver.Capabilities

  -- * Expectations
  , module Test.Hspec.WebDriver.Expectations

  -- * Re-exports from "Test.Hspec"
  , hspec
  , Spec
  , SpecWith
  , describe
  , context
  , it
  , specify
  , parallel
  , runIO

  -- * Hooks
  , module Test.Hspec.WebDriver.Hooks

  -- * Re-exports from "Test.WebDriver"
  , WD
  , Capabilities
  , module Test.WebDriver.Commands
) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Data.Default (Default(..))
import qualified Data.Text as T
import GHC.Stack
import qualified Test.Hspec as H
import Test.Hspec hiding (shouldReturn, shouldBe, shouldSatisfy, shouldThrow, pending, pendingWith, example, before, beforeAll, after)
import Test.WebDriver (WD, Capabilities)
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Capabilities as W
import Test.WebDriver.Commands

import Test.Hspec.WebDriver.Capabilities
import Test.Hspec.WebDriver.Convenience
import Test.Hspec.WebDriver.Core
import Test.Hspec.WebDriver.Expectations
import Test.Hspec.WebDriver.Hooks
import Test.Hspec.WebDriver.Types


#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), Applicative)
import Data.Traversable (traverse)
#endif

-- | A shorthand for constructing a 'WdExample' from a webdriver action when you are only testing a
-- single browser session at once.  See the XKCD example at the top of the page.
runWD :: (HasCallStack) => WD () -> WdExample ()
runWD = WdExample () def

-- | A version of runWD that accepts some custom options
runWDOptions :: (HasCallStack) => WdOptions -> WD () -> WdExample ()
runWDOptions = WdExample ()

-- | Create a webdriver example, specifying which of the multiple sessions the example should be
-- executed against.  I suggest you create an enumeration for multi, for example:
--
-- >data TestUser = Gandolf | Bilbo | Legolas
-- >    deriving (Show, Eq, Enum, Bounded)
-- >
-- >runUser :: TestUser -> WD () -> WDExample TestUser
-- >runUser = runWDWith
-- >
-- >spec :: Spec
-- >spec = session "tests some page" $ using [firefoxCaps] $ do
-- >    it "does something with Gandolf" $ runUser Gandolf $ do
-- >        openPage ...
-- >    it "does something with Bilbo" $ runUser Bilbo $ do
-- >        openPage ...
-- >    it "goes back to the Gandolf session" $ runUser Gandolf $ do
-- >        e <- findElem ....
-- >        ...
--
-- In the above code, two sessions are created and the examples will go back and forth between the
-- two sessions.  Note that a session for Legolas will only be created the first time he shows up in
-- a call to @runUser@, which might be never.  To share information between the sessions (e.g. some
-- data that Gandolf creates that Bilbo should expect), the best way I have found is to use IORefs
-- created with 'runIO' (wrapped in a utility module and inserted into @runUser@).
runWDWith :: (HasCallStack) => multi -> WD () -> WdExample multi
runWDWith multi = WdExample multi def

-- | A version of runWDWith that accepts some custom options
runWDWithOptions :: (HasCallStack) => multi -> WdOptions -> WD () -> WdExample multi
runWDWithOptions = WdExample

-- | A pending example.
pending :: (HasCallStack) => WdExample multi
pending = WdPending Nothing

-- | A pending example with a message.
pendingWith :: (HasCallStack) => String -> WdExample multi
pendingWith = WdPending . Just

-- | A version of 'H.example' which lifts an @IO ()@ to a webdriver example (so it can be composed
-- with other webdriver examples).  In the case of multiple sessions, it doesn't really matter which
-- session the expectation is executed against, so a default value is used.  In the case of single
-- sessions, the type is @WdExample ()@.
example :: (Default multi, HasCallStack) => Expectation -> WdExample multi
example = WdExample def def . liftIO

-- | Combine the examples nested inside this call into a webdriver session or multiple sessions.
-- For each of the capabilities in the list, the examples are executed one at a time in depth-first
-- order and so later examples can rely on the browser state created by earlier examples.  These
-- passes through the examples are independent for different capabilities.  Note that when using
-- 'parallel', the examples within a single pass still execute serially.  Different passes through
-- the examples will be executed in parallel.  The sessions are managed as follows:
--
-- * In the simplest case when @multi@ is @()@, before the first example is executed a new webdriver
-- session with the given capabilities is created.  The examples are then executed in depth-first
-- order, and the session is then closed when either an exception occurs or the examples complete.
-- (The session can be left open with 'inspectSession').
--
-- * More generally, as the examples are executed, each time a new value of type @multi@ is seen, a
-- new webdriver session with the capabilities is automatically created.  Later examples will
-- continue with the session matching their value of @multi@.
--
-- This function uses the default webdriver host (127.0.0.1), port (4444), and basepath
-- (@\/wd\/hub@).
session :: (HasCallStack) => String -> ([Capabilities], SpecWith (WdTestSession multi)) -> Spec
session msg (caps, spec) = sessionWith W.defaultConfig msg (caps', spec)
  where
    caps' = map f caps
    f c = case A.toJSON (W.browser c) of
      A.String b -> (c, T.unpack b)
      _ -> (c, show c) -- this should not be the case, every browser toJSON is a string


-- | A variation of 'session' which allows you to specify the webdriver configuration.  Note that
-- the capabilities in the 'W.WDConfig' will be ignored, instead the capabilities will come from the
-- list of 'Capabilities' passed to 'sessionWith'.
--
-- In addition, each capability is paired with a descriptive string which is passed to hspec to
-- describe the example.  By default, 'session' uses the browser name as the description.  'sessionWith'
-- supports a more detailed description so that in the hspec output you can distinguish between
-- capabilities that share the same browser but differ in the details, for example capabilities with and
-- without javascript.
sessionWith :: (HasCallStack) => W.WDConfig -> String -> ([(Capabilities, String)], SpecWith (WdTestSession multi)) -> Spec
sessionWith cfg msg (caps, spec) = spec'
  where
    procT c = procTestSession cfg (W.getCaps c) spec
    spec' = case caps of
              [] -> it msg $ H.pendingWith "No capabilities specified"
              [(c,cDscr)] -> describe (msg ++ " using " ++ cDscr) $ procT c
              _ -> describe msg $ mapM_ (\(c,cDscr) -> describe ("using " ++ cDscr) $ procT c) caps
