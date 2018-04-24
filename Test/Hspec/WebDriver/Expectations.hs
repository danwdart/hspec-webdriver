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

expectationFailure :: HasCallStack => String -> WD ()
expectationFailure = liftIO . H.expectationFailure

shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> WD ()
x `shouldBe` y = liftIO $ x `H.shouldBe` y

shouldSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> WD ()
shouldSatisfy v p = liftIO $ v `H.shouldSatisfy` p

shouldStartWith :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> WD ()
shouldStartWith list prefix = liftIO $ list `H.shouldStartWith` prefix

shouldEndWith :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> WD ()
shouldEndWith list prefix = liftIO $ list `H.shouldEndWith` prefix

shouldContain :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> WD ()
shouldContain list sublist = liftIO $ list `H.shouldContain` sublist

shouldMatchList :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> WD ()
shouldMatchList xs ys = liftIO $ xs `H.shouldMatchList` ys

shouldReturn :: (HasCallStack, Show a, Eq a) => WD a -> a -> WD ()
shouldReturn action expected = action >>= (`shouldBe` expected)

shouldNotBe :: (HasCallStack, Show a, Eq a) => a -> a -> WD ()
x `shouldNotBe` y = liftIO $ x `H.shouldNotBe` y

shouldNotSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> WD ()
shouldNotSatisfy v p = liftIO $ v `H.shouldNotSatisfy` p

shouldNotContain :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> WD ()
shouldNotContain list sublist = liftIO $ list `H.shouldNotContain` sublist

shouldNotReturn :: (HasCallStack, Show a, Eq a) => WD a -> a -> WD ()
shouldNotReturn action expected = action >>= (`shouldNotBe` expected)

-----------------------------------------------------

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

-- | Asserts that the action throws an exception.
shouldThrow :: (Show e, Eq e, Exception e, HasCallStack) => WD a -> e -> WD ()
shouldThrow w expected = do
  r <- try w
  case r of
    Left err -> err `shouldBe` expected
    Right _ -> liftIO $ assertFailure $ "did not get expected exception " ++ show expected
