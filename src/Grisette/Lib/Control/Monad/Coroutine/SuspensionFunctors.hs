{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Grisette.Lib.Control.Monad.Coroutine.SuspensionFunctors where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Grisette.Core
import Grisette.Lib.Control.Monad
import Grisette.Lib.Control.Monad.Coroutine

instance (SymBoolOp bool, GMergeable bool x, GMergeable bool y) => GMergeable bool (Yield x y) where
  gmergingStrategy = gproduct2Strategy Yield (\(Yield x y) -> (x, y)) gmergingStrategy gmergingStrategy

instance (SymBoolOp bool, GMergeable bool x) => GMergeable1 bool (Yield x) where
  liftGMergingStrategy = gproduct2Strategy Yield (\(Yield x y) -> (x, y)) gmergingStrategy

instance (SymBoolOp bool, GMergeable bool x, GMergeable bool y) => GMergeable bool (Await x y) where
  gmergingStrategy = gwrapStrategy gmergingStrategy Await (\(Await x) -> x)

instance (SymBoolOp bool, GMergeable bool x) => GMergeable1 bool (Await x) where
  liftGMergingStrategy m = gwrapStrategy (liftGMergingStrategy m) Await (\(Await x) -> x)

instance
  (SymBoolOp bool, GMergeable bool req, GMergeable bool res, GMergeable bool x) =>
  GMergeable bool (Request req res x)
  where
  gmergingStrategy = gproduct2Strategy Request (\(Request x y) -> (x, y)) gmergingStrategy gmergingStrategy

instance (SymBoolOp bool, GMergeable bool req, GMergeable bool res) => GMergeable1 bool (Request req res) where
  liftGMergingStrategy m = gproduct2Strategy Request (\(Request x y) -> (x, y)) gmergingStrategy (liftGMergingStrategy m)

mrgYield :: (SymBoolOp bool, GMonadUnion bool m, GMergeable bool x) => x -> Coroutine (Yield x) m ()
mrgYield x = mrgSuspend (Yield x $ mrgReturn ())
{-# INLINEABLE mrgYield #-}

mrgAwait :: (SymBoolOp bool, GMonadUnion bool m, GMergeable bool x) => Coroutine (Await x) m x
mrgAwait = mrgSuspend (Await mrgReturn)
{-# INLINEABLE mrgAwait #-}

mrgRequest :: (SymBoolOp bool, GMonadUnion bool m, GMergeable bool x, GMergeable bool y) => x -> Coroutine (Request x y) m y
mrgRequest x = mrgSuspend (Request x mrgReturn)
{-# INLINEABLE mrgRequest #-}
