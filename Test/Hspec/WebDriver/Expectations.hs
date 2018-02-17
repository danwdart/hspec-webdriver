{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances, DeriveDataTypeable, TypeFamilies, CPP, NamedFieldPuns, ScopedTypeVariables, TupleSections #-}
module Test.Hspec.WebDriver.Expectations where

import Control.Exception.Lifted (try, Exception)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import GHC.Stack
import Test.HUnit (assertEqual, assertFailure)
import qualified Test.Hspec as H
import Test.WebDriver (WD)
import Test.WebDriver.Commands


-- | 'H.shouldBe' lifted into the 'WD' monad.
shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> WD ()
x `shouldBe` y = liftIO $ x `H.shouldBe` y

-- | Asserts that the given element matches the given tag.
shouldBeTag :: (HasCallStack) => Element -> T.Text -> WD ()
e `shouldBeTag` name = do
  t <- tagName e
  liftIO $ assertEqual ("tag of " ++ show e) name t

-- | Asserts that the given element has the given text.
shouldHaveText :: (HasCallStack) => Element -> T.Text -> WD ()
e `shouldHaveText` txt = do
  t <- getText e
  liftIO $ assertEqual ("text of " ++ show e) txt t

-- | Asserts that the given elemnt has the attribute given by @(attr name, value)@.
shouldHaveAttr :: (HasCallStack) => Element -> (T.Text, T.Text) -> WD ()
e `shouldHaveAttr` (a, txt) = do
  t <- attr e a
  liftIO $ assertEqual ("attribute " ++ T.unpack a ++ " of " ++ show e) (Just txt) t

-- | Asserts that the action returns the expected result.
shouldReturn :: (Show a, Eq a, HasCallStack) => WD a -> a -> WD ()
action `shouldReturn` expected = action >>= (\a -> liftIO $ a `H.shouldBe` expected)

-- | Asserts that the action throws an exception.
shouldThrow :: (Show e, Eq e, Exception e, HasCallStack) => WD a -> e -> WD ()
shouldThrow w expected = do
  r <- try w
  case r of
    Left err -> err `shouldBe` expected
    Right _ -> liftIO $ assertFailure $ "did not get expected exception " ++ show expected
