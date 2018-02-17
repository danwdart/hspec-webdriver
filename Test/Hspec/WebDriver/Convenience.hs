{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances, DeriveDataTypeable, TypeFamilies, CPP, NamedFieldPuns, ScopedTypeVariables, TupleSections #-}
module Test.Hspec.WebDriver.Convenience where

import Control.Exception.Lifted
import GHC.Stack
import Test.Hspec
import Test.Hspec.WebDriver.Types
import Test.WebDriver (WD)


-- | A synonym for constructing pairs that allows the word @using@ to be used with 'session' so that the session
-- description reads like a sentence.
--
-- >allBrowsers :: [Capabilities]
-- >allBrowsers = [firefoxCaps, chromeCaps, ieCaps]
-- >
-- >browsersExceptIE :: [Capabilities]
-- >browsersExceptIE = [firefoxCaps, chromeCaps]
-- >
-- >mobileBrowsers :: [Capabilities]
-- >mobileBrowsers = [iphoneCaps, ipadCaps, androidCaps]
-- >
-- >myspec :: Spec
-- >myspec = do
-- >  session "for the home page" $ using allBrowsers $ do
-- >    it "loads the page" $ runWD $ do
-- >        ...
-- >    it "scrolls the carosel" $ runWD $ do
-- >        ...
-- >  session "for the users page" $ using browsersExceptIE $ do
-- >    ...
using :: (HasCallStack) => [caps] -> SpecWith (WdTestSession multi) -> ([caps], SpecWith (WdTestSession multi))
using = (,)

-- | Abort the session without closing the session.
--
-- Normally, 'session' will automatically close the session either when the tests complete without
-- error or when any of the tests within the session throws an error.  When developing the test
-- suite, this can be annoying since closing the session causes the browser window to close.
-- Therefore, while developing the test suite, you can insert a call to 'inspectSession'.  This will
-- immedietly halt the session (all later tests will fail) but will not close the session so that
-- the browser window stays open.
inspectSession :: WD ()
inspectSession = throwIO AbortSession
