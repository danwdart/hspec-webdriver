{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances, DeriveDataTypeable, TypeFamilies, CPP, NamedFieldPuns, ScopedTypeVariables, TupleSections #-}
module Test.Hspec.WebDriver.Types where

import Control.Concurrent
import Control.Exception
import Data.Default (Default(..))
import Data.Typeable (Typeable)
import Test.WebDriver (WD)
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Session as W


-- | The state passed between examples inside the mvars.
data WdTestSession multi = WdTestSession {
    -- | The already created sessions
    stSessionMap :: MVar [(multi, W.WDSession)]
    -- | True if the previous example had an error
  , stPrevHadError :: Bool
    -- | True if the previous example was aborted with 'inspectSession'
  , stPrevAborted :: Bool
    -- | Create a new session
  , stConfig :: W.WDConfig
}

-- | A webdriver example.
--
-- The webdriver action of type @'WD' ()@ should interact with the webpage using commands from
-- "Test.WebDriver.Commands" (which is re-exported from this module) and then use the <#g:2
-- expectations> in this module.  It is possible to split up the spec of a single page into multiple
-- examples where later examples start with the web browser state from the end of the previous
-- example.  This is helpful to keep each individual example small and allows the entire spec to be
-- described at the beginning with pending examples.
--
-- The way this works is that you combine examples into a session using 'session' or 'sessionWith'.
-- A webdriver session is then threaded through all examples in a session so that a later example in
-- the session can rely on the webbrowser state as set up by the previous example.  The type system
-- enforces that every webdriver example must be located within a call to 'session' or
-- 'sessionWith'.  Indeed, a 'WdExample' produces a @'SpecWith' ('WdTestSession' multi)@ which can
-- only be converted to 'Spec' using 'session' or 'sessionWith'.  The reason for the 'WdPending'
-- constructor is so that a pending example can be specified with type @'SpecWith' ('WdTestSession'
-- multi)@ so it can compose with the other webdriver examples.
--
-- The type @multi@ is used when testing multiple sessions at once (e.g. to test multiple
-- interacting users), otherwise it is @()@. Values of this type are used to determine which browser
-- session the example should be executed against.  A new session is created every time a new value
-- of type @multi@ is seen.  Note that the type system enforces that every example within the
-- session has the same type @multi@.
data WdExample multi = WdExample multi WdOptions (WD ()) | WdPending (Maybe String)

newtype WdOptions = WdOptions {
  -- Whether to skip the rest of the tests once one fails
  skipRemainingTestsAfterFailure :: Bool
  }

instance Default WdOptions where
  def = WdOptions { skipRemainingTestsAfterFailure = True }

data AbortSession = AbortSession deriving (Show, Typeable)
instance Exception AbortSession
