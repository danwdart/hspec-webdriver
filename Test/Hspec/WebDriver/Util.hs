{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances, DeriveDataTypeable, TypeFamilies, CPP, NamedFieldPuns, ScopedTypeVariables, TupleSections #-}
module Test.Hspec.WebDriver.Util where

import Control.Monad (void)
import Control.Monad.Trans.State (state, evalState, execState)
import GHC.Stack
import Test.Hspec.Core.Spec (Item(..), SpecTree, Tree(..))


whenJust :: (Monad m) => Maybe a -> (a -> m b) -> m ()
whenJust Nothing _ = return ()
whenJust (Just x) action = void $ action x

-- | Traverse a spec allowing the type to change
traverseTree :: (HasCallStack) => Applicative f => (Item a -> f (Item b)) -> SpecTree a -> f (SpecTree b)
traverseTree f (Leaf i) = Leaf <$> f i
traverseTree f (Node msg ss) = Node msg <$> traverse (traverseTree f) ss
traverseTree f (NodeWithCleanup c ss) = NodeWithCleanup c' <$> traverse (traverseTree f) ss
    where
        c' _b = c undefined -- this undefined is OK since we do not export the definition of WdTestSession
                            -- so the user cannot do anything with the passed in value to 'afterAll'

traverseSpec :: (HasCallStack) => Applicative f => (Item a -> f (Item b)) -> [SpecTree a] -> f [SpecTree b]
traverseSpec f = traverse (traverseTree f)

-- | Process the items in a depth-first walk, passing in the item counter value.
mapWithCounter :: (HasCallStack) => (Int -> Item a -> Item b) -> [SpecTree a] -> [SpecTree b]
mapWithCounter f s = flip evalState 0 $ traverseSpec go s
  where go item = state $ \cnt -> (f cnt item, cnt + 1)

mapNormal :: (HasCallStack) => (Item a -> Item b) -> [SpecTree a] -> [SpecTree b]
mapNormal f s = flip evalState 0 $ traverseSpec go s
  where go item = state $ \cnt -> (f item, cnt + 1)

countItems :: (HasCallStack) => [SpecTree a] -> Int
countItems s = flip execState 0 $ traverseSpec go s
  where go item = state $ \cnt -> (item, cnt+1)
