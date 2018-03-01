{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances, DeriveDataTypeable, TypeFamilies, CPP, NamedFieldPuns, ScopedTypeVariables, TupleSections #-}
module Test.Hspec.WebDriver.Util where

import Control.Monad (void)
import Control.Monad.Trans.State (state, evalState)
import GHC.Stack
import Test.Hspec.Core.Spec (Item(..), SpecTree, Tree(..))


whenJust :: (Monad m) => Maybe a -> (a -> m b) -> m ()
whenJust Nothing _ = return ()
whenJust (Just x) action = void $ action x

-- | Traverse a spec allowing the type to change
traverseTree :: (HasCallStack) => Applicative f => a -> (Item a -> f (Item b)) -> SpecTree a -> f (SpecTree b)
traverseTree _ f (Leaf i) = Leaf <$> f i
traverseTree initial f (Node msg ss) = Node msg <$> traverse (traverseTree initial f) ss
traverseTree initial f (NodeWithCleanup c ss) = NodeWithCleanup c' <$> traverse (traverseTree initial f) ss
    where
        c' _b = c initial

traverseSpec :: (HasCallStack) => Applicative f => a -> (Item a -> f (Item b)) -> [SpecTree a] -> f [SpecTree b]
traverseSpec initial f = traverse (traverseTree initial f)

mapNormal :: (HasCallStack) => a -> (Item a -> Item b) -> [SpecTree a] -> [SpecTree b]
mapNormal initial f s = flip evalState 0 $ traverseSpec initial go s
  where go item = state $ \cnt -> (f item, cnt + 1)
