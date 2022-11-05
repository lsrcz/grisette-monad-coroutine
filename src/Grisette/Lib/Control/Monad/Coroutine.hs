{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Grisette.Lib.Control.Monad.Coroutine where

import Control.Monad.Coroutine hiding (merge)
import Grisette.Core
import Grisette.Lib.Control.Monad

liftCoroEitherMergingStrategy ::
  (SymBoolOp bool, GMergeable1 bool s, GMergeable1 bool m) =>
  GMergingStrategy bool x ->
  GMergingStrategy bool (Either (s (Coroutine s m x)) x)
liftCoroEitherMergingStrategy ms =
  liftGMergingStrategy2 (liftGMergingStrategy (liftGMergingStrategy ms)) ms

coroEitherMergingStrategy ::
  (SymBoolOp bool, GMergeable1 bool s, GMergeable1 bool m, GMergeable bool x) =>
  GMergingStrategy bool (Either (s (Coroutine s m x)) x)
coroEitherMergingStrategy = liftGMergingStrategy2 gmergingStrategy1 gmergingStrategy

instance
  (SymBoolOp bool, GMergeable1 bool m, GMergeable bool a, GMergeable1 bool sus) =>
  GMergeable bool (Coroutine sus m a)
  where
  gmergingStrategy =
    gwrapStrategy
      (liftGMergingStrategy coroEitherMergingStrategy)
      Coroutine
      (\(Coroutine v) -> v)

instance (SymBoolOp bool, GMergeable1 bool m, GMergeable1 bool sus) => GMergeable1 bool (Coroutine sus m) where
  liftGMergingStrategy m =
    gwrapStrategy
      (liftGMergingStrategy $ liftCoroEitherMergingStrategy m)
      Coroutine
      (\(Coroutine v) -> v)

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool a, GMergeable1 bool sus) =>
  GSimpleMergeable bool (Coroutine sus m a)
  where
  gmrgIte = mrgIf

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable1 bool sus) =>
  GSimpleMergeable1 bool (Coroutine sus m)
  where
  liftGMrgIte m = mrgIfWithStrategy (SimpleStrategy m)

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable1 bool sus) =>
  GUnionLike bool (Coroutine sus m)
  where
  mergeWithStrategy s ((Coroutine v) :: Coroutine sus m a) =
    Coroutine $ mergeWithStrategy (liftCoroEitherMergingStrategy s) v
  mrgIfWithStrategy s cond (Coroutine t) (Coroutine f) =
    Coroutine $ mrgIfWithStrategy (liftCoroEitherMergingStrategy s) cond t f
  single x = Coroutine $ single $ Right x
  unionIf cond (Coroutine t) (Coroutine f) =
    Coroutine $ unionIf cond t f

instance
  (Monoid symbolSet, GExtractSymbolics symbolSet (m (Either (sus (Coroutine sus m a)) a))) =>
  GExtractSymbolics symbolSet (Coroutine sus m a)
  where
  gextractSymbolics (Coroutine v) = gextractSymbolics v

mrgSuspend ::
  forall m s bool x.
  (Functor s, GMonadUnion bool m, SymBoolOp bool, GMergeable bool x, GMergeable1 bool s) =>
  s (Coroutine s m x) ->
  Coroutine s m x
mrgSuspend s =
  Coroutine
    $ mergeWithStrategy
      coroEitherMergingStrategy
    $ return (Left s)
{-# INLINEABLE mrgSuspend #-}

mrgMapMonad ::
  forall s m m' bool x.
  (Functor s, SymBoolOp bool, GMergeable1 bool s, GMergeable bool x, Monad m, GMonadUnion bool m') =>
  (forall y. m y -> m' y) ->
  Coroutine s m x ->
  Coroutine s m' x
mrgMapMonad f (Coroutine r) =
  Coroutine
    { resume =
        f r >>= \x ->
          mergeWithStrategy
            coroEitherMergingStrategy
            $ return
            $ map' x
    }
  where
    map' :: Either (s (Coroutine s m x)) x -> Either (s (Coroutine s m' x)) x
    map' (Right r1) = Right r1
    map' (Left s) = Left $ mrgMapMonad f <$> s
{-# INLINEABLE mrgMapMonad #-}

mrgMapSuspension ::
  forall s m bool x s'.
  (Functor s, SymBoolOp bool, GMonadUnion bool m, GMergeable bool x, GMergeable1 bool s') =>
  (forall y. s y -> s' y) ->
  Coroutine s m x ->
  Coroutine s' m x
mrgMapSuspension f (Coroutine r) =
  Coroutine
    { resume =
        r >>= \x ->
          mergeWithStrategy coroEitherMergingStrategy $ return $ map' x
    }
  where
    map' :: Either (s (Coroutine s m x)) x -> Either (s' (Coroutine s' m x)) x
    map' (Right r1) = Right r1
    map' (Left s) = Left $ f $ mrgMapSuspension f <$> s
{-# INLINEABLE mrgMapSuspension #-}

mrgMapFirstSuspension ::
  forall s m bool x.
  (Functor s, SymBoolOp bool, GMergeable1 bool s, GMonadUnion bool m, GMergeable bool x) =>
  (forall y. s y -> s y) ->
  Coroutine s m x ->
  Coroutine s m x
mrgMapFirstSuspension f (Coroutine r) =
  Coroutine
    { resume =
        r >>= \s -> mrgReturnWithStrategy coroEitherMergingStrategy $
          case s of
            Right x -> Right x
            Left x -> Left $ f x
    }

instance GMergeable bool (Naught x) where
  gmergingStrategy = SimpleStrategy gmrgIte

instance GMergeable1 bool Naught where
  liftGMergingStrategy _ = SimpleStrategy gmrgIte

instance GSimpleMergeable bool (Naught x) where
  gmrgIte _ x _ = x

instance GSimpleMergeable1 bool Naught where
  liftGMrgIte _ _ x _ = x

runCoroutine ::
  (SymBoolOp bool, GMonadUnion bool m, GMergeable bool x) =>
  Coroutine Naught m x ->
  m x
runCoroutine (Coroutine r) = do
  v <- r
  case v of
    Left _ -> error "Won't happen"
    Right x -> mrgReturn x

mrgBounce ::
  (Functor s, SymBoolOp bool, GMergeable1 bool s, GMonadUnion bool m, GMergeable bool x) =>
  (s (Coroutine s m x) -> Coroutine s m x) ->
  Coroutine s m x ->
  Coroutine s m x
mrgBounce f (Coroutine r) = Coroutine $ mergeWithStrategy coroEitherMergingStrategy $ do
  r1 <- r
  case r1 of
    Left s -> resume $ f s
    Right x -> return $ Right x

mrgPogoStick ::
  (GMonadUnion bool m, GMergeable bool x) =>
  (s (Coroutine s m x) -> Coroutine s m x) ->
  Coroutine s m x ->
  m x
mrgPogoStick f (Coroutine r) = do
  r1 <- r
  case r1 of
    Left h -> mrgPogoStick f $ f h
    Right v -> mrgReturn v

mrgPogoStickM ::
  (GMonadUnion bool m, GMergeable bool x) =>
  (s (Coroutine s m x) -> m (Coroutine s m x)) ->
  Coroutine s m x ->
  m x
mrgPogoStickM f (Coroutine r) = do
  r1 <- r
  case r1 of
    Left h -> do
      cs <- f h
      mrgPogoStickM f cs
    Right v -> mrgReturn v

mrgFoldRun ::
  (GMonadUnion bool m, SymBoolOp bool, GMergeable bool x, GMergeable bool a) =>
  (a -> s (Coroutine s m x) -> (a, Coroutine s m x)) ->
  a ->
  Coroutine s m x ->
  m (a, x)
mrgFoldRun f a (Coroutine r) = do
  r1 <- r
  case r1 of
    Left s -> case f a s of
      (a1, c1) -> mrgFoldRun f a1 c1
    Right v -> mrgReturn (a, v)

type MrgPairBinder bool m =
  forall x y r. (GMergeable bool r) => (x -> y -> m r) -> m x -> m y -> m r

mrgSequentialBinder :: (SymBoolOp bool, GMonadUnion bool m) => MrgPairBinder bool m
mrgSequentialBinder f ma mb = merge $ do
  a <- ma
  b <- mb
  f a b
