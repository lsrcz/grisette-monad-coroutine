{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Grisette.Lib.Control.Monad.Coroutine.SuspensionFunctors
  ( mrgYield,
    mrgAwait,
    mrgRequest,
  )
where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Grisette.Core
import Grisette.Lib.Control.Monad
import Grisette.Lib.Control.Monad.Coroutine

instance (Mergeable x, Mergeable y) => Mergeable (Yield x y) where
  rootStrategy = product2Strategy Yield (\(Yield x y) -> (x, y)) rootStrategy rootStrategy

instance (Mergeable x) => Mergeable1 (Yield x) where
  liftRootStrategy = product2Strategy Yield (\(Yield x y) -> (x, y)) rootStrategy

instance (Mergeable x, Mergeable y) => Mergeable (Await x y) where
  rootStrategy = wrapStrategy rootStrategy Await (\(Await x) -> x)

instance (Mergeable x) => Mergeable1 (Await x) where
  liftRootStrategy m = wrapStrategy (liftRootStrategy m) Await (\(Await x) -> x)

instance
  (Mergeable req, Mergeable res, Mergeable x) =>
  Mergeable (Request req res x)
  where
  rootStrategy = product2Strategy Request (\(Request x y) -> (x, y)) rootStrategy rootStrategy

instance (Mergeable req, Mergeable res) => Mergeable1 (Request req res) where
  liftRootStrategy m = product2Strategy Request (\(Request x y) -> (x, y)) rootStrategy (liftRootStrategy m)

-- | Symbolic version of 'Control.Monad.Coroutine.SuspensionFunctors.yield',
-- the result would be merged and propagate the mergeable knowledge.
mrgYield :: (MonadUnion m, Mergeable x) => x -> Coroutine (Yield x) m ()
mrgYield x = mrgSuspend (Yield x $ mrgReturn ())
{-# INLINEABLE mrgYield #-}

-- | Symbolic version of 'Control.Monad.Coroutine.SuspensionFunctors.await',
-- the result would be merged and propagate the mergeable knowledge.
mrgAwait :: (MonadUnion m, Mergeable x) => Coroutine (Await x) m x
mrgAwait = mrgSuspend (Await mrgReturn)
{-# INLINEABLE mrgAwait #-}

-- | Symbolic version of 'Control.Monad.Coroutine.SuspensionFunctors.request',
-- the result would be merged and propagate the mergeable knowledge.
mrgRequest :: (MonadUnion m, Mergeable x, Mergeable y) => x -> Coroutine (Request x y) m y
mrgRequest x = mrgSuspend (Request x mrgReturn)
{-# INLINEABLE mrgRequest #-}
