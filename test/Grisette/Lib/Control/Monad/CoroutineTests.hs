{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Grisette.Lib.Control.Monad.CoroutineTests where

import Control.Monad.Coroutine hiding (merge)
import Control.Monad.Coroutine.SuspensionFunctors
import Grisette
import Grisette.Lib.Control.Monad.Coroutine.SuspensionFunctors ()
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertFailure, (@?=))

coroutineTests :: Test
coroutineTests =
  testGroup
    "CoroutineTests"
    [ testGroup
        "MonadUnion for Coroutine"
        [ testCase "merge" $ do
            let Coroutine v =
                  merge
                    ( Coroutine $
                        unionIf
                          "a"
                          (single $ Left $ Yield "b" $ Coroutine $ single $ Right "c")
                          (single $ Left $ Yield "d" $ Coroutine $ single $ Right "e") ::
                        Coroutine (Yield SymBool) UnionM SymBool
                    )
            case v of
              Single (Left (Yield x (Coroutine (Single (Right y))))) -> do
                x @?= symIte "a" "b" "d"
                y @?= symIte "a" "c" "e"
              _ -> assertFailure "Failed to merge Coroutine",
          testCase "mrgReturn" $ do
            case (mrgReturn 1 :: Coroutine (Yield SymBool) UnionM Integer) of
              Coroutine (Single (Right 1)) -> return ()
              _ -> assertFailure "mrgReturn for Coroutine is not working",
          testCase "mrgIf" $ do
            let Coroutine v =
                  mrgIf
                    "a"
                    (Coroutine $ single $ Left $ Yield "b" $ Coroutine $ single $ Right "c")
                    (Coroutine $ single $ Left $ Yield "d" $ Coroutine $ single $ Right "e") ::
                    Coroutine (Yield SymBool) UnionM SymBool
            case v of
              Single (Left (Yield x (Coroutine (Single (Right y))))) -> do
                x @?= symIte "a" "b" "d"
                y @?= symIte "a" "c" "e"
              _ -> assertFailure "Failed to merge Coroutine"
        ],
      testCase "Mergeable for Coroutine" $ do
        let SimpleStrategy s = rootStrategy :: MergingStrategy (Coroutine (Yield SymBool) UnionM SymBool)
        let a1 :: Coroutine (Yield SymBool) UnionM SymBool =
              Coroutine (mrgReturn (Left (Yield "a" (Coroutine (mrgReturn (Right "b"))))))
        let a2 :: Coroutine (Yield SymBool) UnionM SymBool =
              Coroutine (mrgReturn (Left (Yield "c" (Coroutine (mrgReturn (Right "d"))))))
        let Coroutine r = s "e" a1 a2
        case r of
          Single (Left (Yield x (Coroutine (Single (Right y))))) -> do
            x @?= symIte "e" "a" "c"
            y @?= symIte "e" "b" "d"
          _ -> assertFailure "Bad shape",
      testCase "SimpleMergeable for Coroutine" $ do
        let a1 :: Coroutine (Yield SymBool) UnionM SymBool =
              Coroutine (mrgReturn (Left (Yield "a" (Coroutine (mrgReturn (Right "b"))))))
        let a2 :: Coroutine (Yield SymBool) UnionM SymBool =
              Coroutine (mrgReturn (Left (Yield "c" (Coroutine (mrgReturn (Right "d"))))))
        let Coroutine r = mrgIte "e" a1 a2
        let Coroutine r1 = mrgIte1 "e" a1 a2
        let Coroutine ru1 = mrgIf "e" a1 a2
        case r of
          Single (Left (Yield x (Coroutine (Single (Right y))))) -> do
            x @?= symIte "e" "a" "c"
            y @?= symIte "e" "b" "d"
          _ -> assertFailure "Bad shape"
        case r1 of
          Single (Left (Yield x (Coroutine (Single (Right y))))) -> do
            x @?= symIte "e" "a" "c"
            y @?= symIte "e" "b" "d"
          _ -> assertFailure "Bad shape"
        case ru1 of
          Single (Left (Yield x (Coroutine (Single (Right y))))) -> do
            x @?= symIte "e" "a" "c"
            y @?= symIte "e" "b" "d"
          _ -> assertFailure "Bad shape"
    ]
