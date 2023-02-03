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
  (Mergeable1 s, Mergeable1 m) =>
  MergingStrategy x ->
  MergingStrategy (Either (s (Coroutine s m x)) x)
liftCoroEitherMergingStrategy ms =
  liftRootStrategy2 (liftRootStrategy (liftRootStrategy ms)) ms

coroEitherMergingStrategy ::
  (Mergeable1 s, Mergeable1 m, Mergeable x) =>
  MergingStrategy (Either (s (Coroutine s m x)) x)
coroEitherMergingStrategy = liftRootStrategy2 rootStrategy1 rootStrategy

instance
  (Mergeable1 m, Mergeable a, Mergeable1 sus) =>
  Mergeable (Coroutine sus m a)
  where
  rootStrategy =
    wrapStrategy
      (liftRootStrategy coroEitherMergingStrategy)
      Coroutine
      (\(Coroutine v) -> v)

instance (Mergeable1 m, Mergeable1 sus) => Mergeable1 (Coroutine sus m) where
  liftRootStrategy m =
    wrapStrategy
      (liftRootStrategy $ liftCoroEitherMergingStrategy m)
      Coroutine
      (\(Coroutine v) -> v)

instance
  (UnionLike m, Mergeable a, Mergeable1 sus) =>
  SimpleMergeable (Coroutine sus m a)
  where
  mrgIte = mrgIf

instance
  (UnionLike m, Mergeable1 sus) =>
  SimpleMergeable1 (Coroutine sus m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)

instance
  (UnionLike m, Mergeable1 sus) =>
  UnionLike (Coroutine sus m)
  where
  mergeWithStrategy s ((Coroutine v) :: Coroutine sus m a) =
    Coroutine $ mergeWithStrategy (liftCoroEitherMergingStrategy s) v
  mrgIfWithStrategy s cond (Coroutine t) (Coroutine f) =
    Coroutine $ mrgIfWithStrategy (liftCoroEitherMergingStrategy s) cond t f
  single x = Coroutine $ single $ Right x
  unionIf cond (Coroutine t) (Coroutine f) =
    Coroutine $ unionIf cond t f

instance
  (ExtractSymbolics (m (Either (sus (Coroutine sus m a)) a))) =>
  ExtractSymbolics (Coroutine sus m a)
  where
  extractSymbolics (Coroutine v) = extractSymbolics v

mrgSuspend ::
  forall m s x.
  (Functor s, MonadUnion m, Mergeable x, Mergeable1 s) =>
  s (Coroutine s m x) ->
  Coroutine s m x
mrgSuspend s =
  Coroutine
    $ mergeWithStrategy
      coroEitherMergingStrategy
    $ return (Left s)
{-# INLINEABLE mrgSuspend #-}

mrgMapMonad ::
  forall s m m' x.
  (Functor s, Mergeable1 s, Mergeable x, Monad m, MonadUnion m') =>
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
  forall s m x s'.
  (Functor s, MonadUnion m, Mergeable x, Mergeable1 s') =>
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
  forall s m x.
  (Functor s, Mergeable1 s, MonadUnion m, Mergeable x) =>
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

instance Mergeable (Naught x) where
  rootStrategy = SimpleStrategy mrgIte

instance Mergeable1 Naught where
  liftRootStrategy _ = SimpleStrategy mrgIte

instance SimpleMergeable (Naught x) where
  mrgIte _ x _ = x

instance SimpleMergeable1 Naught where
  liftMrgIte _ _ x _ = x

runCoroutine ::
  (MonadUnion m, Mergeable x) =>
  Coroutine Naught m x ->
  m x
runCoroutine (Coroutine r) = do
  v <- r
  case v of
    Left _ -> error "Won't happen"
    Right x -> mrgReturn x

mrgBounce ::
  (Functor s, Mergeable1 s, MonadUnion m, Mergeable x) =>
  (s (Coroutine s m x) -> Coroutine s m x) ->
  Coroutine s m x ->
  Coroutine s m x
mrgBounce f (Coroutine r) = Coroutine $ mergeWithStrategy coroEitherMergingStrategy $ do
  r1 <- r
  case r1 of
    Left s -> resume $ f s
    Right x -> return $ Right x

mrgPogoStick ::
  (MonadUnion m, Mergeable x) =>
  (s (Coroutine s m x) -> Coroutine s m x) ->
  Coroutine s m x ->
  m x
mrgPogoStick f (Coroutine r) = do
  r1 <- r
  case r1 of
    Left h -> mrgPogoStick f $ f h
    Right v -> mrgReturn v

mrgPogoStickM ::
  (MonadUnion m, Mergeable x) =>
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
  (MonadUnion m, Mergeable x, Mergeable a) =>
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
  forall x y r. (Mergeable r) => (x -> y -> m r) -> m x -> m y -> m r

mrgSequentialBinder :: (MonadUnion m) => MrgPairBinder bool m
mrgSequentialBinder f ma mb = merge $ do
  a <- ma
  b <- mb
  f a b
